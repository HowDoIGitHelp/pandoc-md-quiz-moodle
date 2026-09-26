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

asQuestions :: [Block] -> [Question]
asQuestions ((OrderedList (start, prefix, delim) [items]) : rest) =
    error "undefined"

asHTMLFormatText :: [Inline] -> Text
asHTMLFormatText ((Str text) : rest) = text <> (asHTMLFormatText rest)
asHTMLFormatText ((Code _ text) : rest) =
    (pack "<code>") <> text <> (pack "</code>") <> (asHTMLFormatText rest)
asHTMLFormatText ((Math InlineMath text) : rest) =
    (pack "\\[") <> text <> (pack "\\]") <> (asHTMLFormatText rest)
asHTMLFormatText ((Emph inlines) : rest) =
    (pack "<em>") <> (asHTMLFormatText inlines) <> (pack "</em>") <> (asHTMLFormatText rest)
asHTMLFormatText ((Strong inlines) : rest) =
    (pack "<strong>") <> (asHTMLFormatText inlines) <> (pack "</strong>") <> (asHTMLFormatText rest)
asHTMLFormatText (inline : rest) = error ("unsupported inline element" ++ (show inline))
asHTMLFormatText [] = ""

asXMLCDATA :: Text -> Xml Elem
asXMLCDATA text =
    xtextRaw ("<![CDATA[" <> (encodeUtf8Builder text) <> "]]>")

asMoodleTextBlock :: Text -> [Inline] -> Xml Elem
asMoodleTextBlock tagName inlines =
    xelem tagName
        ( xattr "format" "html"
            <#> ( xelem "text"
                ((asXMLCDATA . asHTMLFormatText) inlines) ) )

asChoices :: Block -> [Choice]
asChoices (OrderedList (start, prefix, delim) [items]) =
    map (\x -> Choice x) items

main :: IO ()
main = do
    putStrLn "test"

