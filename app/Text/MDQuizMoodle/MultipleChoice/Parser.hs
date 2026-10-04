{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.MultipleChoice.Parser where

import Text.Pandoc.JSON
import Data.Text (pack, Text, uncons, replace, words)
import Text.MDQuizMoodle.Util.HtmlTextRender (toText)
import Text.MDQuizMoodle.MultipleChoice.Types
import Data.Char (chr, isLower)
import Text.MDQuizMoodle.Util.Helper
import Prelude hiding (words)

toChoices :: Block -> [Choice]
toChoices (OrderedList _ items) = map (\x -> Choice (toText x)) items
toChoices b = error ("unable to parse choices " ++ (show b))

toMultipleChoice :: [Block] -> Block -> MultipleChoice
toMultipleChoice blocks list =
    MultipleChoice "MC Question" (toText blocks) (toChoices list)
toMultipleChoice [] _ = error "Multiple Choice Question must start with question text"

toMultipleChoiceList :: Block -> [MultipleChoice]
toMultipleChoiceList (OrderedList _ items) = map go items
    where
        go q = case (span (not . isChoices) q) of
            (takenParas, (choices@(OrderedList _ _) : [])) ->
                (toMultipleChoice takenParas choices)
            (takenParas, (choices@(OrderedList _ _) : rest)) ->
                toMultipleChoice (takenParas ++ rest) choices
            (takenParas, []) ->
                error ("missing choices")
            (takenParas, _) ->
                error ("unsupported pattern" ++ (show q))
            _ -> error "unsupported pattern"

isChoices :: Block -> Bool
isChoices (OrderedList (1, LowerAlpha, _) _) = True
isChoices _ = False

scoredFeedbackChoiceHelper :: Text -> [Block] -> ChoiceFeedback
scoredFeedbackChoiceHelper score feedback =
    ChoiceFeedback (feedbackScore score) (toText feedback)

toChoiceFeedback :: Bool -> [Block] -> ChoiceFeedback
toChoiceFeedback _ ((Para ((Emph [Str score]):Space:rest1)) : rest2) =
    scoredFeedbackChoiceHelper score ((Para rest1):rest2)
toChoiceFeedback _ [Plain ((Emph [Str score]):Space:rest)] =
    scoredFeedbackChoiceHelper score [Plain rest]
toChoiceFeedback True feedback =
    ChoiceFeedback 100 (toText feedback)
toChoiceFeedback False feedback =
    ChoiceFeedback 0 (toText feedback)
toChoiceFeedback _ _ = error "unable to parse choice feedback"

plainFeedback True = ChoiceFeedback 100.0 "Correct"
plainFeedback False = ChoiceFeedback 0.0 "Incorrect"

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

plainMCFeedbackList :: Text -> [ChoiceFeedback]
plainMCFeedbackList answer =
    map plainFeedback (parseAnswers answer)

toChoiceFeedbackList :: [Bool] -> [[Block]] -> [ChoiceFeedback]
toChoiceFeedbackList answers choicesFeedback =
    zipWith toChoiceFeedback answers' choicesFeedback'
    where
        (answers', choicesFeedback') =
            unzip (paddedZip False [Plain [Str ""]] answers choicesFeedback)

toMCAnswerKeyHelper :: Text -> [Block] -> MultipleChoiceKey
toMCAnswerKeyHelper answer rest =
    case (break isOrderedList rest) of
        (feedback, []) ->
            MultipleChoiceKey (toText feedback) (plainMCFeedbackList answer)
        (feedback, [OrderedList _ items]) ->
            MultipleChoiceKey
                (toText feedback)
                (toChoiceFeedbackList (parseAnswers answer) items)
        (feedback, ((OrderedList _ items):extraFeedback)) ->
            MultipleChoiceKey
                (toText (feedback ++ extraFeedback))
                (toChoiceFeedbackList (parseAnswers answer) items)
        _ -> error "unable to parse answer key"

toMultipleChoiceAnswerKey :: [Block] -> MultipleChoiceKey
toMultipleChoiceAnswerKey ((Para [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMultipleChoiceAnswerKey ((Plain [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMultipleChoiceAnswerKey [Plain ((Strong [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMultipleChoiceAnswerKey [Plain ((Emph [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMultipleChoiceAnswerKey _ = error "answer key must start with the answer"

toMultipleChoiceAnswerKeyList :: Block -> [MultipleChoiceKey]
toMultipleChoiceAnswerKeyList (OrderedList _ items) = map toMultipleChoiceAnswerKey items

