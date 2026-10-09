{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.Cloze.Renderer where

import Text.XML.Generator
import Data.Text (pack)
import Text.MDQuizMoodle.Cloze.Types
import Text.MDQuizMoodle.Util.XMLRender (toTextElemCDATA, toMoodleText)
import Text.MDQuizMoodle.Util.Helper (padEnd)

toMoodleCloze :: Int -> Cloze -> GeneralFeedback -> Xml Elem
toMoodleCloze id (Cloze title questionText)
    (GeneralFeedback generalFeedback) =
    ( xelem "question" $ xattr "type" "cloze" <#> ( xelems
        $ (xelem "name" $ toMoodleText ((pack $ show id) <> " " <> title))
        : ( xelem "questiontext"
            ( xattr "format" "html"
                <#> toTextElemCDATA questionText ) )
        : ( xelem "generalfeedback"
            $ xattr "format" "html"
            <#> toTextElemCDATA generalFeedback )
        : [] ) )
