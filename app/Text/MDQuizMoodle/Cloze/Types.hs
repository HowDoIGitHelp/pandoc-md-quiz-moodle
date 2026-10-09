module Text.MDQuizMoodle.Cloze.Types where

import Data.Text

data Cloze = Cloze Text Text
    deriving (Show)

data GeneralFeedback = GeneralFeedback Text
    deriving (Show)
