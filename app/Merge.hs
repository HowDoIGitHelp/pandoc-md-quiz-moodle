{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc.JSON (toJSONFilter)
import Options.Applicative as O
    ( Parser
    , strOption
    , execParser
    , helper
    , progDesc
    , header
    , fullDesc
    , info
    , help
    , metavar
    , short
    , long
    , auto
    , (<**>) )
import Data.ByteString as BS
    ( readFile
    , ByteString
    , append
    , putStr )
import Data.Aeson (Value, Value(Object))
import Data.Aeson.KeyMap (singleton, union, null)
import Data.Aeson.Key (fromString)
import Data.Frontmatter as FM
import Data.Yaml (encode)
import Prelude hiding (null)

data Args = Args
    { questionsPath :: String
    , answersPath :: String }

argParser :: O.Parser Args
argParser = Args
    <$> strOption
        ( long "questions"
        <> short 'q'
        <> metavar "FILE"
        <> help "path to questions" )
    <*> strOption
        ( long "answers"
        <> short 'a'
        <> metavar "FILE"
        <> help "path to answers" )

-- ast :: FilePath -> Pandoc
-- ast path = do
--     md <- readFile path
--     let frontmatter = def { readerExtensions = pandocExtensions }
--     Pandoc meta blocks <- runIO (readMarkdown frontmatter md)
--     return Pandoc meta blocks

isEmpty :: Value -> Bool
isEmpty (Object fm) = null fm
isEmpty _ = False

combinedFrontMatter :: Value -> Value -> BS.ByteString
combinedFrontMatter fm1 fm2
    | isEmpty fm1 && isEmpty fm2 = ""
    | isEmpty fm1 = "---\n" <> (encode fm2) <> "---\n"
    | isEmpty fm2 = "---\n" <> (encode fm1) <> "---\n"
    | otherwise = "---\n" <> (encode combinedFM) <> "---\n"
    where
        combinedFM = union (singleton (fromString "questions") fm1) 
            (singleton (fromString "answers") fm2)

merge :: FilePath -> FilePath -> IO BS.ByteString
merge file1 file2 = do
    contents1 <- BS.readFile file1
    contents2 <- BS.readFile file2
    let (front1, md1) = case FM.parseYamlFrontmatter contents1 of
            FM.Done md front -> (front :: Value, md)
            _ -> (Object mempty, contents1)
    let (front2, md2) = case FM.parseYamlFrontmatter contents2 of
            FM.Done md front -> (front :: Value, md)
            _ -> (Object mempty, contents2)
    return ((combinedFrontMatter front1 front2) <> md1 <> "\n" <> md2)

main :: IO ()
main = do
    args <- execParser opts
    case args of
        Args { questionsPath = q, answersPath = a } -> do
            mergedDoc <- merge q a
            BS.putStr mergedDoc
    where
        opts = info (argParser <**> helper)
            ( fullDesc
            <> progDesc "Merge two markdown files with respect to frontmatter"
            <> header "md-slides" )
