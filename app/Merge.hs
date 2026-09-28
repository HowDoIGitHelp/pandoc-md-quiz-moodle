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
import Data.Aeson (Value)
import Data.Aeson.KeyMap (singleton, union)
import Data.Aeson.Key (fromString)
import Data.Frontmatter as FM
import Data.Yaml (encode)

data Args = Args
    { questionsPath :: String
    , answersPath :: String }

argParser :: O.Parser Args
argParser = Args
    <$> strOption
        ( long "questions"
        <> short 'q'
        <> metavar "DIR"
        <> help "path to questions" )
    <*> strOption
        ( long "answers"
        <> short 'a'
        <> metavar "DIR"
        <> help "path to answers" )

-- ast :: FilePath -> Pandoc
-- ast path = do
--     md <- readFile path
--     let frontmatter = def { readerExtensions = pandocExtensions }
--     Pandoc meta blocks <- runIO (readMarkdown frontmatter md)
--     return Pandoc meta blocks

merge :: FilePath -> FilePath -> IO BS.ByteString
merge file1 file2 = do
    contents1 <- BS.readFile file1
    contents2 <- BS.readFile file2
    let (front1, md1) = case FM.parseYamlFrontmatter contents1 of
            FM.Done md front -> (front :: Value, md)
            _ -> error "could not parse questions markdown"
    let (front2, md2) = case FM.parseYamlFrontmatter contents2 of
            FM.Done md front -> (front :: Value, md)
            _ -> error "could not parse answers markdown"
    let wrapper1 = singleton (fromString "questions") front1
    let wrapper2 = singleton (fromString "answers") front2
    let unifiedYaml = union wrapper1 wrapper2
    let unifiedFront = "---\n" <> (encode unifiedYaml) <> "---\n"
    return (unifiedFront <> md1 <> "\n" <> md2)

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
