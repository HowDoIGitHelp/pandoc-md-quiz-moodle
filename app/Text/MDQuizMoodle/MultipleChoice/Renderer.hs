{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.MultipleChoice.Renderer where

import Text.XML.Generator
import Data.Text (pack)
import Text.MDQuizMoodle.MultipleChoice.Types
import Text.MDQuizMoodle.MultipleChoice.Parser
import Text.MDQuizMoodle.Util.XMLRender (toTextElemCDATA, toMoodleText)
import Text.MDQuizMoodle.Util.Helper (padEnd)

toMoodleChoice :: Choice -> ChoiceFeedback -> Xml Elem
toMoodleChoice (Choice blocks) (ChoiceFeedback score feedback)=
    xelem "answer" $ xattrs
        [ (xattr "fraction" ((pack . show) score))
        , (xattr "format" "html") ]
        <#> ( xelems
            $ toTextElemCDATA blocks
            : ( xelem "feedback" $ xattr "format" "html"
                <#> toTextElemCDATA feedback )
            : [] )

toMoodleMultipleChoice :: Int -> MultipleChoice -> MultipleChoiceKey -> Xml Elem
toMoodleMultipleChoice id (MultipleChoice title questionText choices)
    (MultipleChoiceKey generalFeedback choicesFeedback) =
    ( xelem "question" $ xattr "type" "multichoice" <#> ( xelems
        $ (xelem "name" $ toMoodleText ((pack $ show id) <> " " <> title))
        : ( xelem "questiontext"
            ( xattr "format" "html"
                <#> toTextElemCDATA questionText ) )
        : ( xelem "generalfeedback"
            $ xattr "format" "html"
            <#> toTextElemCDATA generalFeedback )
        : (toMoodleChoiceList choices choicesFeedback') ) )
    where
        defaultFeedback = ( padEnd
            (length choices)
            (ChoiceFeedback 0.0 "Incorrect")
            choicesFeedback )
        choicesFeedback' = if (length choicesFeedback < length choices)
            then defaultFeedback
            else choicesFeedback

toMoodleChoiceList :: [Choice] -> [ChoiceFeedback] -> [Xml Elem]
toMoodleChoiceList choices cfs = zipWith toMoodleChoice choices cfs
