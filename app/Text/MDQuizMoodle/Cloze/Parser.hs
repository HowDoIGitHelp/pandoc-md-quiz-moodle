{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.Cloze.Parser where

import Text.Pandoc.JSON
import Data.Text (pack, Text, uncons, replace, words)
import Text.MDQuizMoodle.Cloze.HtmlTextRenderSafe (toText)
import Text.MDQuizMoodle.Cloze.Types
import Data.Char (chr, isLower)
import Text.MDQuizMoodle.Util.Helper
import Prelude hiding (words)

toCloze :: [Block] -> Cloze
toCloze blocks = Cloze "Cloze Question" $ toText blocks

toClozeList :: Block -> [Cloze]
toClozeList (OrderedList _ items) = map toCloze items

toGeneralFeedback :: [Block] -> GeneralFeedback
toGeneralFeedback blocks = GeneralFeedback $ toText blocks

toGeneralFeedbackList :: Block -> [GeneralFeedback]
toGeneralFeedbackList (OrderedList _ items) =
    map toGeneralFeedback items
