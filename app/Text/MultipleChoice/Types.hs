module Text.MultipleChoice.Types where

import Data.Text

data Choice = Choice Text
    deriving (Show)

data MultipleChoice = MultipleChoice Text Text [Choice]
    deriving (Show)

data MultipleChoiceKey = MultipleChoiceKey Text [ChoiceFeedback]
    deriving (Show)

data ChoiceFeedback = ChoiceFeedback Float Text
    deriving (Show)


