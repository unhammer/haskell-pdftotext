{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

{- ORMOLU_DISABLE -}
{-|
Module      : Pdftotext.Internal
Description : Internal functions
Copyright   : (c) 2020 G. Eyaeb
License     : BSD-3-Clause
Maintainer  : geyaeb@protonmail.com
Stability   : experimental
Portability : POSIX

Internal functions.
-}
{- ORMOLU_ENABLE -}
module Pdftotext.Internal
  ( -- * Types
    Document (..),
    Layout (..),
    Page (..),
    Properties (..),

    -- * Loading PDF's
    openByteStringIO,
    openFile,

    -- * Document functions
    pageIO,
    pagesIO,
    pagesTotalIO,
    pdftotextIO,
    propertiesIO,

    -- * Page functions
    pageTextIO,
  )
where

import Control.Monad (forM)
import Data.ByteString.Internal
import qualified Data.Text as T
import Foreign (ForeignPtr, newForeignPtr, nullPtr, withForeignPtr)
import Foreign.C (withCString)
import GHC.Generics
import Pdftotext.Foreign

newtype Document = Document (ForeignPtr Poppler_Document)

-- | Document properties.
--
-- @since 0.0.2.0
data Properties = Properties
  { author :: Maybe T.Text,
    creator :: Maybe T.Text,
    keywords :: Maybe T.Text,
    metadata :: Maybe T.Text,
    producer :: Maybe T.Text,
    subject :: Maybe T.Text,
    title :: Maybe T.Text
  }
  deriving (Show, Generic)

data Page = Page
  { -- | Number of this page in original document.
    pageNumber :: Int,
    -- | Total number of pages in original document.
    pageOutOf :: Int,
    -- | Pointer to document which created this page.
    pageDocumentPtr :: ForeignPtr Poppler_Document,
    -- | Pointer to this page.
    pagePtr :: ForeignPtr Poppler_Page
  }

instance Show Page where
  show (Page n o _ _) = "Page " ++ show n ++ "/" ++ show o

-- | Layout of text extracted from PDF.
data Layout
  = -- | Text emulates layout of PDF, including horizontal spaces,
    -- and preserves hyphenation; corresponds to calling @pdftotext -layout@
    Physical
  | -- | Discards horizontal spaces, preserves hyphenation;
    -- corresponds to calling @pdftotext -raw@
    Raw
  | -- | Discards horizontal spaces, removes hyphenation;
    -- corresponds to calling @pdftotext@ without layout argument
    None
  deriving (Eq, Show)

-- | Open PDF from file. If file does not exist or cannot be parsed as valid PDF,
-- `Nothing` is returned.
openFile :: FilePath -> IO (Maybe Document)
openFile file =
  withCString file \cfile -> do
    ptr <- ffiOpenPdf cfile
    if ptr == nullPtr
      then return Nothing
      else Just . Document <$> newForeignPtr ffiDocumentDelete ptr

-- | Open PDF represented as bytestring. If document cannot be parsed as valid PDF,
-- `Nothing` is returned.
openByteStringIO :: ByteString -> IO (Maybe Document)
openByteStringIO (PS bsfptr _ len) =
  withForeignPtr bsfptr \bsptr -> do
    docptr <- ffiOpenData bsptr (fromIntegral len)
    if docptr == nullPtr
      then return Nothing
      else Just . Document <$> newForeignPtr ffiDocumentDelete docptr

-- | Return all pages from document.
pagesIO :: Document -> IO [Page]
pagesIO (Document fptr) = do
  withForeignPtr fptr \ptr -> do
    pageno <- ffiDocumentPages ptr
    forM [0 .. pageno - 1] \pno -> do
      p <- ffiDocumentOpenPage ptr pno >>= newForeignPtr ffiPageDelete
      return $ Page (fromIntegral pno + 1) (fromIntegral pageno) fptr p

-- | Return page number 'no' from PDF document, if the page exists.
pageIO :: Int -> Document -> IO (Maybe Page)
pageIO no doc@(Document fptr) = withForeignPtr fptr \ptr -> do
  pno <- pagesTotalIO doc
  if no > 0 && no <= pno
    then Just . Page no pno fptr <$> (ffiDocumentOpenPage ptr (fromIntegral no - 1) >>= newForeignPtr ffiPageDelete)
    else return Nothing

-- | Return number of pages contained in document.
pagesTotalIO :: Document -> IO Int
pagesTotalIO (Document fptr) =
  fromIntegral <$> withForeignPtr fptr ffiDocumentPages

-- | Extract text from a page with given 'Layout'.
pageTextIO :: Layout -> Page -> IO T.Text
pageTextIO layout (Page _ _ _ fptr) = withForeignPtr fptr \ptr -> asText (ffiPageText ptr l)
  where
    l =
      case layout of
        Raw -> 0
        Physical -> 1
        None -> 2

-- | Extract properties from the document.
--
-- @since 0.0.2.0
propertiesIO :: Document -> IO Properties
propertiesIO (Document fptr) = withForeignPtr fptr \ptr -> do
  a <- asText $ ffiDocumentAuthor ptr
  c <- asText $ ffiDocumentCreator ptr
  k <- asText $ ffiDocumentKeywords ptr
  m <- asText $ ffiDocumentMetadata ptr
  p <- asText $ ffiDocumentProducer ptr
  s <- asText $ ffiDocumentSubject ptr
  t <- asText $ ffiDocumentTitle ptr
  return $ Properties (f a) (f c) (f k) (f m) (f p) (f s) (f t)
  where
    f x =
      if T.null x
        then Nothing
        else Just x

-- | Extract text from PDF document with given 'Layout'.
pdftotextIO :: Layout -> Document -> IO T.Text
pdftotextIO layout doc = do
  ps <- pagesIO doc
  txt <- mapM (pageTextIO layout) ps
  return $ T.concat txt
