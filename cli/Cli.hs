{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson ((.=), ToJSON (..), defaultOptions, genericToJSON, object)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Range (Range, fromRanges, intersection, lbi)
import Data.Range.Parser (parseRanges)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Options.Applicative
import Pdftotext.Internal
import qualified Text.PrettyPrint.ANSI.Leijen as P

data Command
  = Print PrintOptions
  | Info InfoOptions

data PrintOptions = PrintOptions
  { prtFile :: FilePath,
    prtPages :: [Range Int],
    prtOutfile :: Maybe FilePath,
    prtSeparate :: Bool,
    prtColor :: Bool,
    prtViewer :: Bool
  }
  deriving (Show)

data InfoFormat = JsonFormat | PlainFormat deriving (Eq, Show)

data InfoOptions = InfoOptions
  { infFile :: FilePath,
    infFormat :: InfoFormat
  }
  deriving (Show)

data Information = Information
  { iProperties :: Properties,
    iFile :: FilePath,
    iPages :: Int
  }
  deriving (Show)

instance ToJSON Information where
  toJSON Information {..} =
    object
      [ "file" .= iFile,
        "pages" .= iPages,
        "properties" .= genericToJSON defaultOptions iProperties
      ]

main :: IO ()
main =
  execParser
    ( info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Extract text from PDF")
    )
    >>= \case
      Print opts -> printDocument opts
      Info opts -> printInfo opts

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "print" (info printOptions (progDesc "Print extracted text" <> footer "RANGE: -3,5,7-12,15,20-"))
        <> command "info" (info infoOptions (progDesc "Show information about document"))
    )

infoOptions :: Parser Command
infoOptions =
  fmap Info $
    InfoOptions
      <$> strArgument (metavar "FILE" <> help "PDF file")
      <*> option format (long "format" <> short 'f' <> help "Output format (plain, json)" <> value PlainFormat)
  where
    format =
      eitherReader \case
        "json" -> Right JsonFormat
        "plain" -> Right PlainFormat
        f -> Left $ f ++ " is not a valid output format, use one of: plain, json"

printOptions :: Parser Command
printOptions =
  fmap Print $
    PrintOptions
      <$> strArgument (metavar "FILE" <> help "PDF file")
      <*> option range (long "pages" <> short 'p' <> help "Range of pages to process" <> metavar "RANGE" <> value [])
      <*> pure Nothing -- switch (metavar "FILE" <> long "output" <> short 'o' <> help "Write output to file")
      <*> pure False -- switch (long "separate" <> help "Separate pages")
      <*> pure False -- switch (long "color" <> short "c" <> help "Use colors")
      <*> pure False -- switch (long "viewer" <> short "v" <> help "Use internal viewer")
  where
    range = eitherReader (first show . parseRanges)

printDocument :: PrintOptions -> IO ()
printDocument PrintOptions {..} = do
  f <- openFile prtFile
  case f of
    Just d -> do
      pageNo <- pagesTotalIO d
      pages <- mapM (flip pageIO d) (pageList pageNo prtPages)
      txt <- mapM (pageTextIO Physical) (catMaybes pages)
      T.putStrLn (T.concat txt)
    _ -> putStrLn $ prtFile ++ " is not a valid PDF document"

pageList :: Int -> [Range Int] -> [Int]
pageList total [] = [1 .. total]
pageList total ranges =
  sort
    $ filter (<= total)
    $ take total
    $ fromRanges
    $ intersection [lbi 1] ranges

printInfo :: InfoOptions -> IO ()
printInfo InfoOptions {..} = do
  f <- openFile infFile
  case f of
    Just d -> do
      p <- propertiesIO d
      pageno <- pagesTotalIO d
      let i = Information p infFile pageno
      case infFormat of
        JsonFormat -> printInfoJson i
        PlainFormat -> printInfoPlain i
    _ -> putStrLn $ infFile ++ " is not a valid PDF document"

printInfoJson :: Information -> IO ()
printInfoJson p = TL.putStrLn (encodeToLazyText $ toJSON p)

{- ORMOLU_DISABLE -}
printInfoPlain :: Information -> IO ()
printInfoPlain Information{..} =
  P.putDoc $
    P.text "File      :" P.<+> P.text iFile P.<$>
    P.text "Pages     :" P.<+> P.text (show iPages) P.<$>
    P.text "Properties" P.<$>
      P.indent 2 (
          P.text "Title   :" P.<+> P.text (maybe "" T.unpack title)    P.<$>
          P.text "Author  :" P.<+> P.text (maybe "" T.unpack author)   P.<$>
          P.text "Subject :" P.<+> P.text (maybe "" T.unpack subject)  P.<$>
          P.text "Creator :" P.<+> P.text (maybe "" T.unpack creator)  P.<$>
          P.text "Producer:" P.<+> P.text (maybe "" T.unpack producer) P.<$>
          P.text "Keywords:" P.<+> P.text (maybe "" T.unpack keywords)
        ) P.<> P.hardline
  where Properties{..} = iProperties
{- ORMOLU_ENABLE -}
