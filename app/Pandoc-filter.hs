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
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (Html)

data Choice = Choice Text
    deriving (Show)

data Question
    = MultipleChoice Text Text [Choice]
    | ShortAnswer Text Text
    deriving (Show)

data ScoredFeedback
    = ScoredFeedbackChoice Float Text
    | ScoredFeedbackText Float Text Text
    deriving (Show)

data AnswerKey
    = MultipleChoiceKey Text Text [ScoredFeedback]
    | ShortAnswerKey Text [ScoredFeedback]
    deriving (Show)

inlineSpace :: Html
inlineSpace = H.preEscapedToHtml (" " :: Text)

toInlineHTML :: [Inline] -> Html
toInlineHTML (Space : rest) =
    inlineSpace <> (toInlineHTML rest)
toInlineHTML (SoftBreak : rest) =
    inlineSpace <> (toInlineHTML rest)
toInlineHTML ((Str text) : rest) =
    (H.preEscapedToHtml text) <> (toInlineHTML rest)
toInlineHTML ((Code _ text) : rest) =
    (H.code $ H.preEscapedToHtml text) <> (toInlineHTML rest)
toInlineHTML ((Math InlineMath text) : rest) =
    (H.preEscapedToHtml $ "\\(" <> text <> "\\)") <> (toInlineHTML rest)
toInlineHTML ((Math DisplayMath text) : rest) =
    (H.preEscapedToHtml $ "\\[" <> text <> "\\]") <> (toInlineHTML rest)
toInlineHTML ((Emph inlines) : rest) =
    (H.em $ toInlineHTML inlines) <> (toInlineHTML rest)
toInlineHTML ((Strong inlines) : rest) =
    (H.strong $ toInlineHTML inlines) <> (toInlineHTML rest)
toInlineHTML (inline : rest) =
    error ("unsupported inline element " ++ (show inline))
toInlineHTML [] = mempty

toHTMLTableCell :: Cell -> Html
toHTMLTableCell (Cell _ _ _ _ cell) =
    H.td $ toHTMLFormatList cell

toHTMLTableRow :: Row -> Html
toHTMLTableRow (Row _ cells) =
    H.tr $ foldr (>>) mempty (map toHTMLTableCell cells)

toHTMLTableHeadCell :: Cell -> Html
toHTMLTableHeadCell (Cell _ _ _ _ cell) =
    H.th $ toHTMLFormatList cell

toHTMLTableHead :: TableHead -> Html
toHTMLTableHead (TableHead _ [(Row _ cells)]) =
    H.thead $ (H.tr $ foldr (>>) mempty (map toHTMLTableCell cells))

toHTMLTableBody :: TableBody -> Html
toHTMLTableBody (TableBody _ _ _ rows) =
    H.tbody $ foldr (>>) mempty (map toHTMLTableRow rows)

toHTMLFormat :: Block -> Html
toHTMLFormat (Para inlines) = (H.p . toInlineHTML) inlines
toHTMLFormat (Plain inlines) = toInlineHTML inlines
toHTMLFormat (CodeBlock _ text) = (H.pre . H.code . H.preEscapedToHtml) text
toHTMLFormat (BlockQuote blocks) = H.blockquote (toHTMLFormatList blocks)
toHTMLFormat (Para [Math DisplayMath text]) =
    H.preEscapedToHtml ("\\[" <> text <> "\\]")
toHTMLFormat (BulletList items) = H.ul htmlItemsBlock
    where
        htmlItems = map (\li -> H.li (toHTMLFormatList li)) items
        htmlItemsBlock = foldr (>>) mempty htmlItems
toHTMLFormat (Table _ _ _ head [body] _) =
    H.table $ (toHTMLTableHead head) >> (toHTMLTableBody body)
toHTMLFormat block = error ("unsupported block" ++ (show block))

toHTMLFormatList :: [Block] -> Html
toHTMLFormatList blocks = foldr (>>) mempty (map toHTMLFormat blocks)

toText :: [Block] -> Text
toText blocks = (TL.toStrict . renderHtml . toHTMLFormatList) blocks

toTextElemCDATA :: Text -> Xml Elem
toTextElemCDATA text =
    xelem "text" $ toXMLCDATA text

toXMLCDATA :: Text -> Xml Elem
toXMLCDATA text
    | (null text) = xtextRaw " "
    | otherwise = xtextRaw ("<![CDATA[" <> (encodeUtf8Builder text) <> "]]>")

toMoodleText :: Text -> Xml Elem
toMoodleText text = (xelem "text" $ xtext text)

toMoodleChoice :: Choice -> ScoredFeedback -> Xml Elem
toMoodleChoice (Choice blocks) (ScoredFeedbackChoice score feedback)=
    xelem "answer" $ xattrs
        [ (xattr "fraction" ((pack . show) score))
        , (xattr "format" "html") ]
        <#> ( xelems
            $ toTextElemCDATA blocks
            : ( xelem "feedback" $ xattr "format" "html"
                <#> toTextElemCDATA feedback )
            : [] )

toMoodleChoiceList :: [Choice] -> [ScoredFeedback] -> [Xml Elem]
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
        defaultFeedback = toFeedbackList
            (padEnd nChoices False (parseAnswers key)) []
        (generalFeedback', choicesFeedback') = case
            (length choices) == (length choicesFeedback) of
            True ->
                (generalFeedback, choicesFeedback)
            False ->
                ( intercalate (". \n")
                    ( filter
                        (not . null)
                        [ generalFeedback,
                            groupedScoredFeedback choicesFeedback ] )
                , defaultFeedback )
toMoodleQuestion (ShortAnswer title questionText)
    (ShortAnswerKey generalFeedback answers) = error "unsupported"

isEmptyFeedback :: ScoredFeedback -> Bool
isEmptyFeedback (ScoredFeedbackChoice _ feedback) = null feedback

groupedScoredFeedback :: [ScoredFeedback] -> Text
groupedScoredFeedback cfs =
    intercalate ", \n" (map go (filter (not . isEmptyFeedback) cfs))
    where
        go (ScoredFeedbackChoice _ feedback) = feedback

toChoices :: Block -> [Choice]
toChoices (OrderedList _ items) = map (\x -> Choice (toText x)) items
toChoices b = error ("unable to parse choices " ++ (show b))

isParagraph :: Block -> Bool
isParagraph (Para _) = True
isParagraph _ = False

toMCQuestion :: [Block] -> Block -> Question
toMCQuestion blocks list =
    MultipleChoice "Multiple Choice Question" (toText blocks) (toChoices list)
toMCQuestion [] _ = error "Multiple Choice Question must start with question text"

toSAQuestion :: [Block] -> Question
toSAQuestion blocks =
    ShortAnswer "Short Answer Question" (toText blocks)

toQuestions :: [[Block]] -> [Question]
toQuestions items = map go items
    where
        go q = case (span (not . isChoices) q) of
            (takenParas, (choices@(OrderedList _ _) : [])) ->
                (toMCQuestion takenParas choices)
            (takenParas, (choices@(OrderedList _ _) : rest)) ->
                toSAQuestion (takenParas ++ [choices]  ++ rest)
            (takenParas, []) ->
                toSAQuestion takenParas
            (takenParas, _) -> error ("unsupported pattern" ++ (show q))
            _ -> error "unsupported pattern"

isChoices :: Block -> Bool
isChoices (OrderedList (1, LowerAlpha, _) _) = True
isChoices _ = False

feedbackScore :: Text -> Float
feedbackScore score = case ((readMaybe . unpack $ score) :: Maybe Float) of
    Just float -> float * 100.0
    Nothing -> 0.0

scoredFeedbackChoiceHelper :: Text -> [Block] -> ScoredFeedback
scoredFeedbackChoiceHelper score feedback =
    ScoredFeedbackChoice (feedbackScore score) (toText feedback)

toFeedback :: Bool -> [Block] -> ScoredFeedback
toFeedback _ ((Para ((Emph [Str score]):Space:rest1)) : rest2) =
    scoredFeedbackChoiceHelper score ((Para rest1):rest2)
toFeedback _ [Plain ((Emph [Str score]):Space:rest)] =
    scoredFeedbackChoiceHelper score [Plain rest]
toFeedback True feedback =
    ScoredFeedbackChoice 100 (toText feedback)
toFeedback False feedback =
    ScoredFeedbackChoice 0 (toText feedback)
toFeedback _ _ = error "unable to parse choice feedback"

toFeedbackList :: [Bool] -> [[Block]] -> [ScoredFeedback]
toFeedbackList answers [] =
    zipWith toFeedback answers (map defaultFeedback answers)
toFeedbackList answers choicesFeedback =
    zipWith toFeedback
        (padEnd (length choicesFeedback) False answers)
        choicesFeedback

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
        bools = map
            (\choice -> (elem (pack [chr choice]) answersList))
            [122,121..97]
        answersBool = reverse (dropWhile not bools)

toMCAnswerKeyHelper :: Text -> [Block] -> AnswerKey
toMCAnswerKeyHelper answer rest =
    case (break isOrderedList rest) of
        (feedback, []) ->
            MultipleChoiceKey answer (toText feedback) []
        (feedback, [OrderedList _ items]) ->
            MultipleChoiceKey
                answer
                (toText feedback)
                (toFeedbackList (parseAnswers answer) items)
        (feedback, ((OrderedList _ items):extraFeedback)) ->
            MultipleChoiceKey
                answer
                (toText (feedback ++ extraFeedback))
                (toFeedbackList (parseAnswers answer) items)
        _ -> error "unable to parse answer key"

toMCAnswerKey :: [Block] -> AnswerKey
toMCAnswerKey ((Para [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMCAnswerKey ((Plain [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMCAnswerKey [Plain ((Strong [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey [Plain ((Emph [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey _ = error "answer key must start with the answer"

isBulletList :: Block -> Bool
isBulletList (BulletList _) = True
isBulletList _ = False

parseShortAnswers :: Text -> [Text]
parseShortAnswers answers =
    words (replace "," " " answers)

toShortAnswerFeedback :: Block -> [ScoredFeedback]
toShortAnswerFeedback (BulletList items) =
    map go items
    where
        go [Plain ((Emph str) : rest)] =
            ScoredFeedbackText 100 (toText [Plain str]) (toText [Plain rest])
        go [Plain ((Strong str) : rest)] =
            ScoredFeedbackText 100 (toText [Plain str]) (toText [Plain rest])
        go ((Para inlines) : rest) =
            ScoredFeedbackText 100 (toText [Plain inlines]) (toText rest)
        go _ = error "not a valid short answer format"

extractFeedbackList :: [ScoredFeedback] -> [Text]
extractFeedbackList feedbackList =
    map go feedbackList
    where
        go (ScoredFeedbackText _ ans _) = ans

toShortAnswerKeyHelper :: Text -> [Block] -> AnswerKey
toShortAnswerKeyHelper answers rest =
    case (break isBulletList rest) of
        (feedback,[]) ->
            ShortAnswerKey (toText feedback)
                ( map (\ans -> ScoredFeedbackText 100.0 ans "correct")
                    (parseShortAnswers answers) )
        (feedback, [list@(BulletList items)]) ->
            ShortAnswerKey (toText feedback) (toShortAnswerFeedback list)
        ([list@(BulletList items)], []) ->
            ShortAnswerKey "" (toShortAnswerFeedback list)

toShortAnswerKey :: [Block] -> AnswerKey
toShortAnswerKey [Plain ((Strong [Str answers]) : rest)] =
    toShortAnswerKeyHelper answers [Plain rest]

toAnswerKeyList :: Block -> [AnswerKey]
toAnswerKeyList (OrderedList _ items) = map toMCAnswerKey items

isOrderedList :: Block -> Bool
isOrderedList (OrderedList _ _) = True
isOrderedList _ = False

moodleXMLFilter :: Pandoc -> Pandoc
moodleXMLFilter (Pandoc meta blocks) = Pandoc (Meta mempty) [Plain [Str flattenedXMLText]]
    where
        (olist1 : validRest) = case (break isOrderedList blocks) of
            (_, []) -> error "missing questions list"
            (_, rest@(_:_)) -> rest
        olist2 = case (break isOrderedList validRest) of
            (_, []) -> error "cannot parse answer key"
            (_, [x]) -> x
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

