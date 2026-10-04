import Data.Text (Text)

data Choice = Choice Text
    deriving (Show)

data Question
    = MultipleChoice Text Text [Choice]
    | ShortAnswer Text Text
    deriving (Show)

groupedScoredFeedback :: [ScoredFeedback] -> Text
groupedScoredFeedback cfs =
    intercalate ", \n" (map go (filter (not . isEmptyFeedback) cfs))
    where
        go (ScoredFeedbackChoice _ feedback) = feedback
        go (ScoredFeedbackText _ _ feedback) = feedback

