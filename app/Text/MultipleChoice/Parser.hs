{-# Language OverloadedStrings #-}

module Text.MultipleChoice.Parser where

import Text.Pandoc.JSON
import Data.Text (pack, Text, uncons, replace, words)
import Text.Util.HtmlTextRender (toText)
import Text.MultipleChoice.Types
import Data.Char (chr, isLower)
import Text.Util.Helper
import Prelude hiding (words)

toChoices :: Block -> [Choice]
toChoices (OrderedList _ items) = map (\x -> Choice (toText x)) items
toChoices b = error ("unable to parse choices " ++ (show b))

toMCQuestion :: [Block] -> Block -> MultipleChoice
toMCQuestion blocks list =
    MultipleChoice "MC Question" (toText blocks) (toChoices list)
toMCQuestion [] _ = error "Multiple Choice Question must start with question text"

toQuestions :: [[Block]] -> [MultipleChoice]
toQuestions items = map go items
    where
        go q = case (span (not . isChoices) q) of
            (takenParas, (choices@(OrderedList _ _) : [])) ->
                (toMCQuestion takenParas choices)
            (takenParas, (choices@(OrderedList _ _) : rest)) ->
                toMCQuestion (takenParas ++ rest) choices
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

toFeedback :: Bool -> [Block] -> ChoiceFeedback
toFeedback _ ((Para ((Emph [Str score]):Space:rest1)) : rest2) =
    scoredFeedbackChoiceHelper score ((Para rest1):rest2)
toFeedback _ [Plain ((Emph [Str score]):Space:rest)] =
    scoredFeedbackChoiceHelper score [Plain rest]
toFeedback True feedback =
    ChoiceFeedback 100 (toText feedback)
toFeedback False feedback =
    ChoiceFeedback 0 (toText feedback)
toFeedback _ _ = error "unable to parse choice feedback"

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

toFeedbackList :: [Bool] -> [[Block]] -> [ChoiceFeedback]
toFeedbackList answers choicesFeedback =
    zipWith toFeedback answers' choicesFeedback'
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
                (toFeedbackList (parseAnswers answer) items)
        (feedback, ((OrderedList _ items):extraFeedback)) ->
            MultipleChoiceKey
                (toText (feedback ++ extraFeedback))
                (toFeedbackList (parseAnswers answer) items)
        _ -> error "unable to parse answer key"

toMCAnswerKey :: [Block] -> MultipleChoiceKey
toMCAnswerKey ((Para [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMCAnswerKey ((Plain [Str answer]) : rest) | isValidKey answer =
    toMCAnswerKeyHelper answer rest
toMCAnswerKey [Plain ((Strong [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey [Plain ((Emph [Str answer]) : rest)] | isValidKey answer =
    toMCAnswerKeyHelper answer [Plain rest]
toMCAnswerKey _ = error "answer key must start with the answer"

toAnswerKeyList :: Block -> [MultipleChoiceKey]
toAnswerKeyList (OrderedList _ items) = map toMCAnswerKey items

