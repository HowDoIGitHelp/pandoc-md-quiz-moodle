{-# Language OverloadedStrings #-}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.XML.Generator
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8Builder)

data Choice = Choice Block

data Question
    = MultipleChoice Text [Block] [Choice]
    | ShortAnswer Text [Block] [Block]

toQuestions :: [Block] -> [Question]
toQuestions ((OrderedList (start, prefix, delim) [items]) : rest) =
    error "undefined"

toHTMLFormatText :: [Inline] -> Text
toHTMLFormatText ((Str text) : rest) = text <> (toHTMLFormatText rest)
toHTMLFormatText ((Code _ text) : rest) =
    (pack "<code>") <> text <> (pack "</code>") <> (toHTMLFormatText rest)
toHTMLFormatText ((Math InlineMath text) : rest) =
    (pack "\\[") <> text <> (pack "\\]") <> (toHTMLFormatText rest)
toHTMLFormatText ((Emph inlines) : rest) =
    (pack "<em>") <> (toHTMLFormatText inlines) <> (pack "</em>") <> (toHTMLFormatText rest)
toHTMLFormatText ((Strong inlines) : rest) =
    (pack "<strong>") <> (toHTMLFormatText inlines) <> (pack "</strong>") <> (toHTMLFormatText rest)
toHTMLFormatText (inline : rest) = error ("unsupported inline element" ++ (show inline))
toHTMLFormatText [] = ""

toXMLCDATA :: Text -> Xml Elem
toXMLCDATA text =
    xtextRaw ("<![CDATA[" <> (encodeUtf8Builder text) <> "]]>")

toMoodleTextBlock :: Text -> [Inline] -> Xml Elem
toMoodleTextBlock tagName inlines =
    xelem tagName
        ( xattr "format" "html"
            <#> ( xelem "text"
                ((toXMLCDATA . toHTMLFormatText) inlines) ) )

toChoices :: Block -> [Choice]
toChoices (OrderedList (start, prefix, delim) [items]) =
    map (\x -> Choice x) items


main :: IO ()
main = do
    putStrLn "test"

