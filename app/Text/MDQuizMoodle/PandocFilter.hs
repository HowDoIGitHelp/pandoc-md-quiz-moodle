{-# Language OverloadedStrings #-}

module Main where

import Text.Pandoc.JSON
import Text.MDQuizMoodle.MultipleChoice.Parser (toMultipleChoiceList, toMultipleChoiceAnswerKeyList)
import Text.MDQuizMoodle.MultipleChoice.Renderer (toMoodleMultipleChoice)
import Text.MDQuizMoodle.ShortAnswer.Parser (toShortAnswer, toShortAnswerKeyList)
import Text.MDQuizMoodle.ShortAnswer.Renderer (toMoodleShortAnswer)
import Data.Text.Encoding (decodeUtf8)
import Text.XML.Generator (xrender, doc, xelem, defaultDocInfo, xelems)
import Text.MDQuizMoodle.Util.Helper (isOrderedList)
import Data.Text (replace)


moodleXMLFilter :: Pandoc -> Pandoc
moodleXMLFilter (Pandoc meta blocks) = Pandoc (Meta mempty) [Plain [Str flattenedXMLText]]
    where
        (olist1 : validRest) = case (break isOrderedList blocks) of
            (_, []) -> error "missing questions list"
            (_, rest@(_:_)) -> rest
        olist2 = case (break isOrderedList validRest) of
            (_, []) -> error "cannot parse answer key"
            (_, [x]) -> x
            (_, (x:rest)) -> error "there is extra content at the end of the answer key"
        (OrderedList _ questionItems) = olist1
        (OrderedList _ answerItems) = olist2
        questions = toMultipleChoiceList olist1
        answerKeys = toMultipleChoiceAnswerKeyList olist2
        moodleQuestions = zipWith3 toMoodleMultipleChoice [1..(length questions)] questions answerKeys
        renderedXML = xrender (doc defaultDocInfo (xelem "quiz" (xelems moodleQuestions)))
        flattenedXMLText = replace "\n>" ">" (decodeUtf8 renderedXML)

main :: IO ()
main = toJSONFilter moodleXMLFilter

