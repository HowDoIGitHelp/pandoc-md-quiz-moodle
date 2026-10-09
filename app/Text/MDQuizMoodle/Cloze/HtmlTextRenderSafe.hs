{-# Language OverloadedStrings #-}

module Text.MDQuizMoodle.Cloze.HtmlTextRenderSafe where

import Text.Pandoc.JSON
import Data.Text (Text, replace)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (Html)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Text.MDQuizMoodle.Util.HtmlTextRender as HText
import Text.Blaze.Html5.Attributes as A

toClozeText :: Text -> Text
toClozeText = (replace "{" "&#123;") . (replace "}" "&#125;")

toInlineHTML :: [Inline] -> Html
toInlineHTML (Space : rest) =
    HText.inlineSpace <> (toInlineHTML rest)
toInlineHTML (SoftBreak : rest) =
    HText.inlineSpace <> (toInlineHTML rest)
toInlineHTML ((Str text) : rest) =
    (H.preEscapedToHtml text) <> (toInlineHTML rest)
toInlineHTML ((Code _ text) : rest) =
    (H.code $ H.preEscapedToHtml (toClozeText text)) <> (toInlineHTML rest)
toInlineHTML ((Math InlineMath text) : rest) =
    (H.preEscapedToHtml $ "\\(" <> (toClozeText text) <> "\\)") <> (toInlineHTML rest)
toInlineHTML ((Math DisplayMath text) : rest) =
    (H.preEscapedToHtml $ "\\[" <> (toClozeText text) <> "\\]") <> (toInlineHTML rest)
toInlineHTML ((Emph inlines) : rest) =
    (H.em $ toInlineHTML inlines) <> (toInlineHTML rest)
toInlineHTML ((Strong inlines) : rest) =
    (H.strong $ toInlineHTML inlines) <> (toInlineHTML rest)
toInlineHTML (inline : rest) =
    error ("unsupported inline element " ++ (show inline))
toInlineHTML [] = mempty

toHTMLFormat :: Block -> Html
toHTMLFormat (Para inlines) = (H.p . toInlineHTML) inlines
toHTMLFormat (Plain inlines) = toInlineHTML inlines
toHTMLFormat (CodeBlock _ text) = (H.pre . H.code . H.preEscapedToHtml) (toClozeText text)
toHTMLFormat (BlockQuote blocks) = H.blockquote (toHTMLFormatList blocks)
toHTMLFormat (Para [Math DisplayMath text]) =
    (H.p . H.preEscapedToHtml) ("\\[" <> (toClozeText (toClozeText text)) <> "\\]")
toHTMLFormat (BulletList items) = H.ul htmlItemsBlock
    where
        htmlItems = HText.htmlItemsList items
        htmlItemsBlock = foldr (>>) mempty htmlItems
toHTMLFormat (Table _ _ _ head [body] _) =
    H.table $ (HText.toHTMLTableHead head) >> (HText.toHTMLTableBody body)
toHTMLFormat (OrderedList (_, listStyle, _) items) =
    H.ol H.! A.type_ (HText.orderedListType listStyle) $ htmlItemsBlock
    where
        htmlItems = HText.htmlItemsList items
        htmlItemsBlock = foldr (>>) mempty htmlItems
toHTMLFormat block = error ("unsupported block" ++ (show block))

toHTMLFormatList :: [Block] -> Html
toHTMLFormatList blocks = foldr (>>) mempty (map toHTMLFormat blocks)

toText :: [Block] -> Text
toText blocks = (TL.toStrict . renderHtml . toHTMLFormatList) blocks
