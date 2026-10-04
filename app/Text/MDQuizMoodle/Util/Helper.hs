module Text.MDQuizMoodle.Util.Helper where

import Text.Pandoc.JSON
import Data.Text (unpack, Text)
import Text.Read (readMaybe)

padEnd :: Int -> a -> [a] -> [a]
padEnd n padding list = list ++ (replicate (n - length list) padding)

isParagraph :: Block -> Bool
isParagraph (Para _) = True
isParagraph _ = False

feedbackScore :: Text -> Float
feedbackScore score = case ((readMaybe . unpack $ score) :: Maybe Float) of
    Just float -> float * 100.0
    Nothing -> 0.0

isBulletList :: Block -> Bool
isBulletList (BulletList _) = True
isBulletList _ = False

isOrderedList :: Block -> Bool
isOrderedList (OrderedList _ _) = True
isOrderedList _ = False

paddedZip :: a -> b -> [a] -> [b] -> [(a,b)]
paddedZip _ _ [] [] = []
paddedZip defA defB  (x:xs) [] =
    (x, defB) : (paddedZip defA defB xs [])
paddedZip defA defB  [] (y:ys) =
    (defA, y) : (paddedZip defA defB [] ys)
paddedZip defA defB (x:xs) (y:ys) =
    (x, y) : (paddedZip defA defB xs ys)

