module Text.MDQuizMoodle.ShortAnswer.Types where

import Data.Text

data ShortAnswer = ShortAnswer Text Text
    deriving (Show)

data ShortAnswerKey = ShortAnswerKey Text [ShortAnswerFeedback]
    deriving (Show)

data ShortAnswerFeedback = ShortAnswerFeedback Float Text Text
    deriving (Show)

