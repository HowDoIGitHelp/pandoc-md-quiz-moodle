{-# Language OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.XML.Generator
import Data.Text (Text, pack, intercalate, replace)
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict, fromStrict, Text)

data Choice = Choice [Block]
    deriving (Show)

data Question
    = MultipleChoice Text [Block] [Choice]
    | ShortAnswer Text [Block] [Block]
    deriving (Show)

toHTMLFormatText :: [Inline] -> Text
toHTMLFormatText (Space : rest) = (pack " ") <> (toHTMLFormatText rest)
toHTMLFormatText (SoftBreak : rest) = (pack " ") <> (toHTMLFormatText rest)
toHTMLFormatText ((Str text) : rest) = text <> (toHTMLFormatText rest)
toHTMLFormatText ((Code _ text) : rest) =
    (pack "<code>") <> text <> (pack "</code>") <> (toHTMLFormatText rest)
toHTMLFormatText ((Math InlineMath text) : rest) =
    (pack "\\(") <> text <> (pack "\\)") <> (toHTMLFormatText rest)
toHTMLFormatText ((Math DisplayMath text) : rest) =
    (pack "\\[") <> text <> (pack "\\]") <> (toHTMLFormatText rest)
toHTMLFormatText ((Emph inlines) : rest) =
    (pack "<em>") <> (toHTMLFormatText inlines) <> (pack "</em>") <> (toHTMLFormatText rest)
toHTMLFormatText ((Strong inlines) : rest) =
    (pack "<strong>") <> (toHTMLFormatText inlines) <> (pack "</strong>") <> (toHTMLFormatText rest)
toHTMLFormatText (inline : rest) = error ("unsupported inline element " ++ (show inline))
toHTMLFormatText [] = ""

toHTMLFormat :: Block -> Text
toHTMLFormat (Para inlines) = (pack "<p>") <> (toHTMLFormatText inlines) <> (pack "</p>")
toHTMLFormat (Plain inlines) = (pack "<p>") <> (toHTMLFormatText inlines) <> (pack "</p>")
toHTMLFormat (CodeBlock _ text) = (pack "<pre><code>") <> text <> (pack "</code></pre>")
toHTMLFormat (BlockQuote blocks) = (pack "<quote>") <> (toHTMLFormatList blocks) <> (pack "</quote>")
toHTMLFormat (Para [Math DisplayMath text]) = (pack "\\[") <> text <> (pack "\\]")
toHTMLFormat _ = error "unsupported block"

toHTMLFormatList :: [Block] -> Text
toHTMLFormatList blocks = intercalate (pack "\n") (map toHTMLFormat blocks)

toTextElemCDATA :: Text -> Xml Elem
toTextElemCDATA text =
    xelem "text" $ toXMLCDATA text

toXMLCDATA :: Text -> Xml Elem
toXMLCDATA text =
    xtextRaw ("<![CDATA[" <> (encodeUtf8Builder text) <> "]]>")

toMoodleTextBlock :: Text -> [Inline] -> [Xml Elem] -> Xml Elem
toMoodleTextBlock tagName inlines children =
    xelem tagName
        ( xattr "format" "html"
            <#> ( xelem "text"
                ((toXMLCDATA . toHTMLFormatText) inlines) ) )

toMoodleText :: Text -> Xml Elem
toMoodleText text = (xelem "text" $ xtext text)

toMoodleChoice :: Choice -> Xml Elem
toMoodleChoice (Choice blocks) =
    xelem "answer" $ xattrs
        [ (xattr "fraction" "0")
        , (xattr "format" "html") ]
        <#> ( xelems
            $ ((toTextElemCDATA . toHTMLFormatList) blocks)
            : ( xelem "feedback" $ xattr "format" "html"
                <#> toMoodleText "Incorrect" )
            : [] )

toMoodleChoiceList :: [Choice] -> [Xml Elem]
toMoodleChoiceList choices = map toMoodleChoice choices

toMoodleQuestion :: Question -> Xml Elem
toMoodleQuestion (MultipleChoice title blocks choices) =
    xelem "question" $ xattr "type" "multichoice" <#> ( xelems
        $ (xelem "name" $ toMoodleText title)
        : ( xelem "questiontext"
            ( xattr "format" "html"
                <#> (toTextElemCDATA . toHTMLFormatList) blocks ) )
        : (toMoodleChoiceList choices) )

toChoices :: Block -> [Choice]
toChoices (OrderedList _ items) = map (\x -> Choice x) items
toChoices b = error ("unable to parse choices " ++ (show b))

isParagraph :: Block -> Bool
isParagraph (Para _) = True
isParagraph _ = False

toMCQuestion :: [Block] -> Block -> Question
toMCQuestion paraList list =
    MultipleChoice (pack "Multiple Choice Question") paraList (toChoices list)
toMCQuestion [] _ = error "Multiple Choice Question must start with question text"

toQuestions :: [[Block]] -> [Question]
toQuestions items = map go items
    where
        go q = case (span (not . isOrderedList) q) of
            (takenParas, (list@(OrderedList _ _) : [])) ->
                (toMCQuestion takenParas list)
            (takenParas, _) -> error ("unsupported pattern" ++ (show q))
            _ -> error "unsupported pattern"

isOrderedList :: Block -> Bool
isOrderedList (OrderedList _ _) = True
isOrderedList _ = False

moodleXMLFilter :: Pandoc -> Pandoc
moodleXMLFilter (Pandoc meta blocks) = Pandoc (Meta mempty) [Plain [Str flattenedXMLText]]
    where
        (olist1 : validRest) = case (break isOrderedList blocks) of
            (_, rest@(_:_)) -> rest
            (_, []) -> error "missing questions list"
        olist2 = case (break isOrderedList validRest) of
            (_, (x:_)) -> x
            (_, []) -> error "missing answers list"
        (OrderedList _ items) = olist1
        questions = toQuestions items
        moodleQuestions = map toMoodleQuestion questions
        renderedXML = xrender (doc defaultDocInfo (xelem "quiz" (xelems moodleQuestions)))
        flattenedXMLText= replace (pack "\n>") (pack ">") (decodeUtf8 renderedXML)


main :: IO ()
main = toJSONFilter moodleXMLFilter

