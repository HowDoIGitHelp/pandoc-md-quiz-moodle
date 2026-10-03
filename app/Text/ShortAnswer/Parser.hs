
toMoodleQuestion id (ShortAnswer title questionText)
    (ShortAnswerKey generalFeedback answers) = error "unsupported"

toSAQuestion :: [Block] -> Question
toSAQuestion blocks =
    ShortAnswer "Short Answer Question" (toText blocks)

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
