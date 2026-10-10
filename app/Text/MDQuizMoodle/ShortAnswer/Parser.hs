{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.ShortAnswer.Parser where

import Text.Pandoc.JSON
import Text.MDQuizMoodle.ShortAnswer.Types
import Text.MDQuizMoodle.Util.HtmlTextRender (toText)
import Text.MDQuizMoodle.Util.Helper
import Data.Text (Text, replace, words)
import Prelude hiding (words)

toShortAnswer :: [Block] -> ShortAnswer
toShortAnswer blocks =
    ShortAnswer "Short Answer toShortAnswer" (toText blocks)

toShortAnswerList :: Block -> [ShortAnswer]
toShortAnswerList (OrderedList _ items) =
    map toShortAnswer items

parseShortAnswers :: Text -> [Text]
parseShortAnswers answers =
    words (replace "," " " answers)

toShortAnswerFeedback :: Block -> [ShortAnswerFeedback]
toShortAnswerFeedback (BulletList items) =
    map go items
    where
        go [Plain ((Emph str) : rest)] =
            ShortAnswerFeedback 100.0 (toText [Plain str]) (toText [Plain rest])
        go [Plain ((Strong str) : rest)] =
            ShortAnswerFeedback 100.0 (toText [Plain str]) (toText [Plain rest])
        go ((Para inlines) : rest) =
            ShortAnswerFeedback 100.0 (toText [Plain inlines]) (toText rest)
        go _ = error "not a valid short answer format"

extractFeedbackList :: [ShortAnswerFeedback] -> [Text]
extractFeedbackList feedbackList =
    map go feedbackList
    where
        go (ShortAnswerFeedback _ ans _) = ans

toShortAnswerKeyHelper :: Text -> [Block] -> ShortAnswerKey
toShortAnswerKeyHelper answers rest =
    case (break isBulletList rest) of
        (feedback,[]) ->
            ShortAnswerKey (toText feedback)
                ( map (\ans -> ShortAnswerFeedback 100.0 ans "correct")
                    (parseShortAnswers answers) )
        (feedback, [list@(BulletList items)]) ->
            ShortAnswerKey (toText feedback) (toShortAnswerFeedback list)
        ([list@(BulletList items)], []) ->
            ShortAnswerKey "" (toShortAnswerFeedback list)
        _ -> error $ "unsupported pattern" ++ (show $ break isBulletList rest)

toShortAnswerKey :: [Block] -> ShortAnswerKey
toShortAnswerKey [Plain ((Strong [Str answer]) : rest)] =
    toShortAnswerKeyHelper answer [Plain rest]
toShortAnswerKey ((Para [Strong inlines]) : rest) =
    toShortAnswerKeyHelper (toText [Plain inlines]) rest
toShortAnswerKey [] = error "empty answer key"
toShortAnswerKey blocks =
    error $ "unsupported pattern" ++ (show blocks)

toShortAnswerKeyList :: Block -> [ShortAnswerKey]
toShortAnswerKeyList (OrderedList _ items) =
    map toShortAnswerKey items
