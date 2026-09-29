{-# Language OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.XML.Generator
import Data.Text (Text, pack, unpack, intercalate, replace, words, uncons, null)
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict, fromStrict, Text)
import Text.Read (readMaybe)
import Debug.Trace (trace)
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
toHTMLFormatText (Space : rest) = " " <> (toHTMLFormatText rest)
toHTMLFormatText (SoftBreak : rest) = " " <> (toHTMLFormatText rest)
toHTMLFormatText ((Str text) : rest) = text <> (toHTMLFormatText rest)
toHTMLFormatText ((Code _ text) : rest) =
    "<code>" <> text <> "</code>" <> (toHTMLFormatText rest)
toHTMLFormatText ((Math InlineMath text) : rest) =
    "\\(" <> text <> (pack "\\)") <> (toHTMLFormatText rest)
toHTMLFormatText ((Math DisplayMath text) : rest) =
    "\\[" <> text <> "\\]" <> (toHTMLFormatText rest)
toHTMLFormatText ((Emph inlines) : rest) =
    "<em>" <> (toHTMLFormatText inlines) <> "</em>" <> (toHTMLFormatText rest)
toHTMLFormatText ((Strong inlines) : rest) =
    "<strong>" <> (toHTMLFormatText inlines) <> "</strong>" <> (toHTMLFormatText rest)
toHTMLFormatText (inline : rest) = error ("unsupported inline element " ++ (show inline))
toHTMLFormatText [] = ""

toHTMLTableCell :: Cell -> Text
toHTMLTableCell (Cell _ _ _ _ cell) = "<td>" <> (toHTMLFormatList cell) <> "</td>"

toHTMLTableRow :: Row -> Text
toHTMLTableRow (Row _ cells) =
    "<tr>" <> (intercalate " " (map toHTMLTableCell cells)) <> "</tr>"

toHTMLTableHeadCell :: Cell -> Text
toHTMLTableHeadCell (Cell _ _ _ _ cell) = "<th>" <> (toHTMLFormatList cell) <> "</th>"

toHTMLTableHead :: TableHead -> Text
toHTMLTableHead (TableHead _ [(Row _ cells)]) =
    "<thead><tr>" <> (intercalate " " (map toHTMLTableHeadCell cells)) <> "</tr></thead>"

toHTMLTableBody :: TableBody -> Text
toHTMLTableBody (TableBody _ _ _ rows) =
    "<tbody>" <> (intercalate "\n" (map toHTMLTableRow rows)) <> "</tbody>"

toHTMLFormat :: Block -> Text
toHTMLFormat (Para inlines) = "<p>" <> (toHTMLFormatText inlines) <> "</p>"
toHTMLFormat (Plain inlines) = toHTMLFormatText inlines
toHTMLFormat (CodeBlock _ text) = "<pre><code>" <> text <> "</code></pre>"
toHTMLFormat (BlockQuote blocks) = "<quote>" <> (toHTMLFormatList blocks) <> "</quote>"
toHTMLFormat (Para [Math DisplayMath text]) = "\\[" <> text <> "\\]"
toHTMLFormat (BulletList items) = "<ul>" <> htmlItemsBlock <> "</ul>"
    where
        htmlItems = map (\li -> "<li>" <> (toHTMLFormatList li) <> "</li>") items
        htmlItemsBlock = intercalate "\n" htmlItems
toHTMLFormat (Table _ _ _ head [body] _) =
    "<table>" <> (toHTMLTableHead head) <> "\n"
        <> (toHTMLTableBody body) <> "</table>"
toHTMLFormat block = error ("unsupported block" ++ (show block))

toHTMLFormatList :: [Block] -> Text
toHTMLFormatList blocks = intercalate "\n" (map toHTMLFormat blocks)

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

toMoodleChoice :: Choice -> ChoiceFeedback -> Xml Elem
toMoodleChoice (Choice blocks) (ChoiceFeedback score feedback)=
    xelem "answer" $ xattrs
        [ (xattr "fraction" ((pack . show) score))
        , (xattr "format" "html") ]
        <#> ( xelems
            $ toTextElemCDATA blocks
            : ( xelem "feedback" $ xattr "format" "html"
                <#> toTextElemCDATA feedback )
            : [] )

toMoodleChoiceList :: [Choice] -> [ChoiceFeedback] -> [Xml Elem]
toMoodleChoiceList choices cfs = zipWith toMoodleChoice choices cfs

padEnd :: Int -> a -> [a] -> [a]
padEnd n padding list = list ++ (replicate (n - length list) padding)

toMoodleQuestion :: Question -> AnswerKey -> Xml Elem
toMoodleQuestion (MultipleChoice title questionText choices)
    (MultipleChoiceKey key generalFeedback choicesFeedback) =
    ( xelem "question" $ xattr "type" "multichoice" <#> ( xelems
        $ (xelem "name" $ toMoodleText title)
        : ( xelem "questiontext"
            ( xattr "format" "html"
                <#> toTextElemCDATA questionText ) )
        : ( xelem "generalfeedback"
            $ xattr "format" "html"
            <#> toTextElemCDATA generalFeedback' )
        : (toMoodleChoiceList choices choicesFeedback') ) )
    where
        nChoices = (length choices)
        defaultFeedback = toFeedbackList (padEnd nChoices False (parseAnswers key)) []
        (generalFeedback', choicesFeedback') = case (length choices) == (length choicesFeedback) of
            True ->
                (generalFeedback, choicesFeedback)
            False ->
                ( intercalate (". \n") (filter (not . null) [generalFeedback, groupedChoiceFeedback choicesFeedback])
                , defaultFeedback )

isEmptyFeedback :: ChoiceFeedback -> Bool
isEmptyFeedback (ChoiceFeedback _ feedback) = null feedback

groupedChoiceFeedback :: [ChoiceFeedback] -> Text
groupedChoiceFeedback cfs =
    intercalate ", \n" (map go (filter (not . isEmptyFeedback) cfs))
    where
        go (ChoiceFeedback _ feedback) = feedback

toChoices :: Block -> [Choice]
toChoices (OrderedList _ items) = map (\x -> Choice (toHTMLFormatList x)) items
toChoices b = error ("unable to parse choices " ++ (show b))

isParagraph :: Block -> Bool
isParagraph (Para _) = True
isParagraph _ = False

toMCQuestion :: [Block] -> Block -> Question
toMCQuestion paraList list =
    MultipleChoice "Multiple Choice Question" (toHTMLFormatList paraList) (toChoices list)
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
            Just float -> float * 100.0
            Nothing -> 0.0

toFeedback :: Bool -> [Block] -> ChoiceFeedback
toFeedback _ ((Para ((Emph [Str score]):Space:rest1)) : rest2) =
    choiceFeedbackHelper score ((Para rest1):rest2)
toFeedback _ [Plain ((Emph [Str score]):Space:rest)] =
    choiceFeedbackHelper score [Plain rest]
toFeedback True feedback =
    ChoiceFeedback 100 (toHTMLFormatList feedback)
toFeedback False feedback =
    ChoiceFeedback 0 (toHTMLFormatList feedback)
toFeedback _ _ = error "unable to parse choice feedback"

toFeedbackList :: [Bool] -> [[Block]] -> [ChoiceFeedback]
toFeedbackList answers [] =
    zipWith toFeedback answers (map defaultFeedback answers)
toFeedbackList answers choicesFeedback =
    zipWith toFeedback (padEnd (length choicesFeedback) False answers) choicesFeedback

defaultFeedback True = [Plain [Str "Correct"]]
defaultFeedback False = [Plain [Str "Incorrect"]]

isValidKey :: Text -> Bool
isValidKey text =
    case (uncons text) of
        Just (c,_) -> isLower c
        Nothing -> False

parseAnswers :: Text -> [Bool]
parseAnswers answer = answersBool
    where
        answersClean = replace "," " " answer
        answersList = words answersClean
        bools = map (\choice -> (elem (pack [chr choice]) answersList)) [122,121..97]
        answersBool = reverse (dropWhile not bools)

toMCAnswerKeyHelper :: Text -> [Block] -> AnswerKey
toMCAnswerKeyHelper answer rest =
    case (break isOrderedList rest) of
        (feedback, []) ->
            MultipleChoiceKey answer (toHTMLFormatList feedback) []
        (feedback, [OrderedList _ items]) ->
            MultipleChoiceKey answer (toHTMLFormatList feedback) (toFeedbackList (parseAnswers answer) items)
        (feedback, ((OrderedList _ items):extraFeedback)) ->
            MultipleChoiceKey answer (toHTMLFormatList (feedback ++ extraFeedback)) (toFeedbackList (parseAnswers answer) items)
        _ -> error "unable to parse answer key"

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
            (_, []) -> error "missing questions list"
            (_, rest@(_:_)) -> rest
        olist2 = case (break isOrderedList validRest) of
            (_, []) -> error "cannot parse answer key"
            (_, (x:[])) -> x
            (_, (x:rest)) -> error "there is extra content at the end of the answer key"
        (OrderedList _ questionItems) = olist1
        (OrderedList _ answerItems) = olist2
        questions = toQuestions questionItems
        answerKeys = toAnswerKeyList olist2
        moodleQuestions = zipWith toMoodleQuestion questions answerKeys
        renderedXML = xrender (doc defaultDocInfo (xelem "quiz" (xelems moodleQuestions)))
        flattenedXMLText = replace "\n>" ">" (decodeUtf8 renderedXML)

main :: IO ()
main = toJSONFilter moodleXMLFilter

