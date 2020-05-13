{-# LANGUAGE BlockArguments #-}

{- ORMOLU_DISABLE -}
{-|
Module      : Pdftotext
Description : Extracts text from PDF using poppler
Copyright   : (c) 2020 G. Eyaeb
License     : BSD-3-Clause
Maintainer  : geyaeb@protonmail.com
Stability   : experimental
Portability : POSIX

=== Usage

> import qualified Data.Text.IO as T
> import Pdftotext
>
> main :: IO ()
> main = do
>   Just pdf <- openFile "path/to/file.pdf"
>   T.putStrLn $ pdftotext Physical pdf

-}
{- ORMOLU_ENABLE -}
module Pdftotext
  ( -- * Types
    Document,
    Layout (..),
    Page,

    -- * Loading PDF's
    openByteString,
    openFile,

    -- * Document functions
    page,
    pages,
    pagesTotal,
    pdftotext,

    -- * Page functions
    pageNumber,
    pageOutOf,
    pageText,
  )
where

import Data.ByteString
import Data.Text (Text)
import GHC.IO (unsafePerformIO)
import Pdftotext.Internal

-- | Open PDF represented as bytestring. If document cannot be parsed as valid PDF,
-- `Nothing` is returned.
openByteString :: ByteString -> Maybe Document
openByteString = unsafePerformIO . openByteStringIO

-- | Return page number 'no' from PDF document, if the page exists.
page :: Int -> Document -> Maybe Page
page no doc = unsafePerformIO $ pageIO no doc

-- | Return all pages from document.
pages :: Document -> [Page]
pages = unsafePerformIO . pagesIO

-- | Return number of pages contained in document.
pagesTotal :: Document -> Int
pagesTotal = unsafePerformIO . pagesTotalIO

-- | Extract text from PDF document with given 'Layout'.
pdftotext :: Layout -> Document -> Text
pdftotext lay doc = unsafePerformIO $ pdftotextIO lay doc

-- | Extract text from a page with given 'Layout'.
pageText :: Layout -> Page -> Text
pageText l p = unsafePerformIO $ pageTextIO l p
