{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.ShortAnswer.Renderer where

import Text.XML.Generator
import Data.Text (pack)
import Text.MDQuizMoodle.ShortAnswer.Types
import Text.MDQuizMoodle.Util.XMLRender (toTextElemCDATA, toMoodleText)
import Text.MDQuizMoodle.Util.Helper (padEnd)

toMoodleShortAnswerFeedback :: ShortAnswerFeedback -> Xml Elem
toMoodleShortAnswerFeedback (ShortAnswerFeedback score answer feedback)=
    xelem "answer" $ xattrs
        [ (xattr "fraction" ((pack . show) score))
        , (xattr "format" "html") ]
        <#> ( xelems
            $ toTextElemCDATA answer
            : ( xelem "feedback" $ xattr "format" "html"
                <#> toTextElemCDATA feedback )
            : [] )

toMoodleShortAnswer :: Int -> ShortAnswer -> ShortAnswerKey -> Xml Elem
toMoodleShortAnswer id (ShortAnswer title questionText)
    (ShortAnswerKey generalFeedback answersFeedback) =
    ( xelem "question" $ xattr "type" "shortanswer" <#> ( xelems
        $ (xelem "name" $ toMoodleText ((pack $ show id) <> " " <> title))
        : ( xelem "questiontext"
            ( xattr "format" "html"
                <#> toTextElemCDATA questionText ) )
        : ( xelem "generalfeedback"
            $ xattr "format" "html"
            <#> toTextElemCDATA generalFeedback )
        : (map toMoodleShortAnswerFeedback answersFeedback) ) )
