{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.Util.HtmlTextRender where

import Text.Pandoc.JSON
import Data.Text (Text)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (Html)
import qualified Data.Text.Lazy as TL (toStrict)

inlineSpace :: Html
inlineSpace = H.preEscapedToHtml (" " :: Text)

toInlineHTML :: [Inline] -> Html
toInlineHTML (Space : rest) =
    inlineSpace <> (toInlineHTML rest)
toInlineHTML (SoftBreak : rest) =
    inlineSpace <> (toInlineHTML rest)
toInlineHTML ((Str text) : rest) =
    (H.preEscapedToHtml text) <> (toInlineHTML rest)
toInlineHTML ((Code _ text) : rest) =
    (H.code $ H.preEscapedToHtml text) <> (toInlineHTML rest)
toInlineHTML ((Math InlineMath text) : rest) =
    (H.preEscapedToHtml $ "\\(" <> text <> "\\)") <> (toInlineHTML rest)
toInlineHTML ((Math DisplayMath text) : rest) =
    (H.preEscapedToHtml $ "\\[" <> text <> "\\]") <> (toInlineHTML rest)
toInlineHTML ((Emph inlines) : rest) =
    (H.em $ toInlineHTML inlines) <> (toInlineHTML rest)
toInlineHTML ((Strong inlines) : rest) =
    (H.strong $ toInlineHTML inlines) <> (toInlineHTML rest)
toInlineHTML (inline : rest) =
    error ("unsupported inline element " ++ (show inline))
toInlineHTML [] = mempty

toHTMLTableCell :: Cell -> Html
toHTMLTableCell (Cell _ _ _ _ cell) =
    H.td $ toHTMLFormatList cell

toHTMLTableRow :: Row -> Html
toHTMLTableRow (Row _ cells) =
    H.tr $ foldr (>>) mempty (map toHTMLTableCell cells)

toHTMLTableHeadCell :: Cell -> Html
toHTMLTableHeadCell (Cell _ _ _ _ cell) =
    H.th $ toHTMLFormatList cell

toHTMLTableHead :: TableHead -> Html
toHTMLTableHead (TableHead _ [(Row _ cells)]) =
    H.thead $ (H.tr $ foldr (>>) mempty (map toHTMLTableCell cells))

toHTMLTableBody :: TableBody -> Html
toHTMLTableBody (TableBody _ _ _ rows) =
    H.tbody $ foldr (>>) mempty (map toHTMLTableRow rows)

toHTMLFormat :: Block -> Html
toHTMLFormat (Para inlines) = (H.p . toInlineHTML) inlines
toHTMLFormat (Plain inlines) = toInlineHTML inlines
toHTMLFormat (CodeBlock _ text) = (H.pre . H.code . H.preEscapedToHtml) text
toHTMLFormat (BlockQuote blocks) = H.blockquote (toHTMLFormatList blocks)
toHTMLFormat (Para [Math DisplayMath text]) =
    (H.p . H.preEscapedToHtml) ("\\[" <> text <> "\\]")
toHTMLFormat (BulletList items) = H.ul htmlItemsBlock
    where
        htmlItems = map (\li -> H.li (toHTMLFormatList li)) items
        htmlItemsBlock = foldr (>>) mempty htmlItems
toHTMLFormat (Table _ _ _ head [body] _) =
    H.table $ (toHTMLTableHead head) >> (toHTMLTableBody body)
toHTMLFormat block = error ("unsupported block" ++ (show block))

toHTMLFormatList :: [Block] -> Html
toHTMLFormatList blocks = foldr (>>) mempty (map toHTMLFormat blocks)

toText :: [Block] -> Text
toText blocks = (TL.toStrict . renderHtml . toHTMLFormatList) blocks
