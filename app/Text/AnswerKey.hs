import Data.Text

data AnswerKey
    = MultipleChoiceKey Text [ScoredFeedback]
    | ShortAnswerKey Text [ScoredFeedback]
    deriving (Show)

data ScoredFeedback
    = ScoredFeedbackChoice Float Text
    | ScoredFeedbackText Float Text Text
    deriving (Show)

isEmptyFeedback :: ScoredFeedback -> Bool
isEmptyFeedback (ScoredFeedbackChoice _ feedback) = null feedback
isEmptyFeedback (ScoredFeedbackText _ _ feedback) = null feedback

