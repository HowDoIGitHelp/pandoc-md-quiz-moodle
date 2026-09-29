{-# Language OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.XML.Generator
import Data.Text (Text, pack, unpack, intercalate, replace, words, uncons, null)
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict, fromStrict, Text)
import Text.Read (readMaybe)
import Data.Char (chr, isLower)
import Prelude hiding (words, null)

data Choice = Choice Text
    deriving (Show)

data Question
    = MultipleChoice Text Text [Choice]
    | ShortAnswer Text Text Text
    deriving (Show)

data ChoiceFeedback
    = ChoiceFeedback Float Text
    deriving (Show)

data AnswerKey
    = MultipleChoiceKey Text Text [ChoiceFeedback]
    | ShortAnswerKey [Text] [Float]
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
toHTMLFormat (BulletList items) = (pack "<ul>") <> htmlItemsBlock <> (pack "</ul>")
    where
        htmlItems = map (\li -> (pack "<li>") <> (toHTMLFormatList li) <> (pack "</li>")) items
        htmlItemsBlock = intercalate (pack "\n") htmlItems
toHTMLFormat block = error ("unsupported block" ++ (show block))

toHTMLFormatList :: [Block] -> Text
toHTMLFormatList blocks = intercalate (pack "\n") (map toHTMLFormat blocks)

toTextElemCDATA :: Text -> Xml Elem
toTextElemCDATA text =
    xelem "text" $ toXMLCDATA text

toXMLCDATA :: Text -> Xml Elem
toXMLCDATA text
    | (null text) = xtextRaw " "
    | otherwise = xtextRaw ("<![CDATA[" <> (encodeUtf8Builder text) <> "]]>")

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
            $ toTextElemCDATA blocks
            : ( xelem "feedback" $ xattr "format" "html"
                <#> toMoodleText "Incorrect" )
            : [] )

toMoodleChoiceList :: [Choice] -> [Xml Elem]
toMoodleChoiceList choices = map toMoodleChoice choices

toMoodleQuestion :: Question -> AnswerKey -> Xml Elem
toMoodleQuestion (MultipleChoice title questionText choices)
    (MultipleChoiceKey key generalFeedback choicesFeedback) =
    xelem "question" $ xattr "type" "multichoice" <#> ( xelems
        $ (xelem "name" $ toMoodleText title)
        : ( xelem "questiontext"
            ( xattr "format" "html"
                <#> toTextElemCDATA questionText ) )
        : ( xelem "generalfeedback"
            $ xattr "format" "html"
            <#> toTextElemCDATA generalFeedback )
        : (toMoodleChoiceList choices) )

toChoices :: Block -> [Choice]
toChoices (OrderedList _ items) = map (\x -> Choice (toHTMLFormatList x)) items
toChoices b = error ("unable to parse choices " ++ (show b))

isParagraph :: Block -> Bool
isParagraph (Para _) = True
isParagraph _ = False

toMCQuestion :: [Block] -> Block -> Question
toMCQuestion paraList list =
    MultipleChoice (pack "Multiple Choice Question") (toHTMLFormatList paraList) (toChoices list)
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

choiceFeedbackHelper :: Text -> [Block] -> ChoiceFeedback
choiceFeedbackHelper score feedback =
    ChoiceFeedback feedbackScore (toHTMLFormatList feedback)
    where
        feedbackScore = case ((readMaybe . unpack $ score) :: Maybe Float) of
            Just float -> float
            Nothing -> 0.0

toFeedback :: Bool -> [Block] -> ChoiceFeedback
toFeedback _ ((Para ((Emph [Str score]):Space:rest1)) : rest2) =
    choiceFeedbackHelper score ((Para rest1):rest2)
toFeedback _ [Plain ((Emph [Str score]):Space:rest)] =
    choiceFeedbackHelper score [Plain rest]
toFeedback True feedback =
    ChoiceFeedback 1.0 (toHTMLFormatList feedback)
toFeedback False feedback =
    ChoiceFeedback 0.0 (toHTMLFormatList feedback)
toFeedback _ _ = error "unable to parse choice feedback"

toFeedbackList :: [Bool] -> [[Block]] -> [ChoiceFeedback]
toFeedbackList answers choicesFeedback =
    zipWith toFeedback answers choicesFeedback
toFeedbaclList answers [] =
    zipWith toFeedback answers defaultFeedback
    where
        defaultFeedback = map go answers
        go True = [Plain [Str "Correct"]]
        go False = [Plain [Str "Incorrect"]]

isValidKey :: Text -> Bool
isValidKey text =
    case (uncons text) of
        Just (c,_) -> isLower c
        Nothing -> False

toMCAnswerKeyHelper :: Text -> [Block] -> AnswerKey
toMCAnswerKeyHelper answer rest =
    case (break isOrderedList rest) of
        (feedback, []) ->
            MultipleChoiceKey answer (toHTMLFormatList feedback) []
        (feedback, [OrderedList _ items]) ->
            MultipleChoiceKey answer (toHTMLFormatList feedback) (toFeedbackList answersBool items)
        (feedback, ((OrderedList _ items):extraFeedback)) ->
            MultipleChoiceKey answer (toHTMLFormatList (feedback ++ extraFeedback)) (toFeedbackList answersBool items)
        _ -> error "unable to parse answer key"
    where
        answersClean = replace (pack ",") (pack " ") answer
        answersList = words answersClean
        bools = map (\choice -> (elem (pack [chr choice]) answersList)) [122..97]
        answersBool = reverse (dropWhile not bools)

toMCAnswerKey :: [Block] -> AnswerKey
toMCAnswerKey ((Para [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMCAnswerKey ((Plain [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMCAnswerKey [Plain (Str answer : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey [Plain ((Strong [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey [Plain ((Emph [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey _ = error "answer key must start with the answer"

toAnswerKeyList :: Block -> [AnswerKey]
toAnswerKeyList (OrderedList _ items) = map toMCAnswerKey items

moodleXMLFilter :: Pandoc -> Pandoc
moodleXMLFilter (Pandoc meta blocks) = Pandoc (Meta mempty) [Plain [Str flattenedXMLText]]
    where
        (olist1 : validRest) = case (break isOrderedList blocks) of
            (_, rest@(_:_)) -> rest
            (_, []) -> error "missing questions list"
        olist2 = case (break isOrderedList validRest) of
            (_, (x:_)) -> x
            (_, []) -> error "missing answers list"
        (OrderedList _ questionItems) = olist1
        (OrderedList _ answerItems) = olist2
        questions = toQuestions questionItems
        answerKeys = toAnswerKeyList olist2
        moodleQuestions = zipWith toMoodleQuestion questions answerKeys
        renderedXML = xrender (doc defaultDocInfo (xelem "quiz" (xelems moodleQuestions)))
        flattenedXMLText = replace (pack "\n>") (pack ">") (decodeUtf8 renderedXML)

main :: IO ()
main = toJSONFilter moodleXMLFilter

