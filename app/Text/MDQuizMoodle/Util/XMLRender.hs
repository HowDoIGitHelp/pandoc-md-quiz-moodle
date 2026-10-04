{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.Util.XMLRender where

import Text.XML.Generator
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)
import Data.Text (Text, null)
import Prelude hiding (null)

toTextElemCDATA :: Text -> Xml Elem
toTextElemCDATA text =
    xelem "text" $ toXMLCDATA text

toXMLCDATA :: Text -> Xml Elem
toXMLCDATA text
    | (null text) = xtextRaw " "
    | otherwise = xtextRaw ("<![CDATA[" <> (encodeUtf8Builder text) <> "]]>")

toMoodleText :: Text -> Xml Elem
toMoodleText text = (xelem "text" $ xtext text)

