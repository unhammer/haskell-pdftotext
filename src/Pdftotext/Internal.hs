{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

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
import Pdftotext.Foreign

#ifdef XMLC
import qualified Text.XML as X
import qualified Data.Text.Lazy as TL
#endif

newtype Document = Document (ForeignPtr Poppler_Document)

-- | Document properties.
--
-- If flag @xml-conduit@ is set, 'metadata' is of type @Maybe Text.XML.Document@.
--
-- @since 0.0.2.0
data Properties = Properties
  { author :: Maybe T.Text,
    creator :: Maybe T.Text,
    keywords :: Maybe T.Text,
#ifdef XMLC
    metadata :: Maybe X.Document,
#else
    metadata :: Maybe T.Text,
#endif
    producer :: Maybe T.Text,
    subject :: Maybe T.Text,
    title :: Maybe T.Text
  }
  deriving (Show)

data Page = Page
  { -- | Number of this page in original document.
    pageNumber :: Int,
    -- | Total number of pages in original document.
    pageOutOf :: Int,
    pagePtr :: ForeignPtr Poppler_Page
  }

instance Show Page where
  show (Page n o _) = "Page " ++ show n ++ "/" ++ show o

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
    docptr <- ffiOpenPdf cfile
    if docptr == nullPtr
      then return Nothing
      else Just . Document <$> newForeignPtr ffiDocumentDelete docptr

-- | Open PDF represented as bytestring. If document cannot be parsed as valid PDF,
-- `Nothing` is returned.
openByteStringIO :: ByteString -> IO (Maybe Document)
openByteStringIO (PS ptr _ len) =
  withForeignPtr ptr \d -> do
    docptr <- ffiOpenData d (fromIntegral len)
    if docptr == nullPtr
      then return Nothing
      else Just . Document <$> newForeignPtr ffiDocumentDelete docptr

-- | Return all pages from document.
pagesIO :: Document -> IO [Page]
pagesIO (Document doc) = do
  withForeignPtr doc \docptr -> do
    pageno <- ffiDocumentPages docptr
    forM [0 .. pageno - 1] \pno -> do
      p <- ffiDocumentOpenPage docptr pno >>= newForeignPtr ffiPageDelete
      return $ Page (fromIntegral pno + 1) (fromIntegral pageno) p

-- | Return page number 'no' from PDF document, if the page exists.
pageIO :: Int -> Document -> IO (Maybe Page)
pageIO no d@(Document docptr) = withForeignPtr docptr \ptr -> do
  pno <- pagesTotalIO d
  if no > 0 && no <= pno
    then Just . Page no pno <$> (ffiDocumentOpenPage ptr (fromIntegral no - 1) >>= newForeignPtr ffiPageDelete)
    else return Nothing

-- | Return number of pages contained in document.
pagesTotalIO :: Document -> IO Int
pagesTotalIO (Document doc) =
  fromIntegral <$> withForeignPtr doc ffiDocumentPages

-- | Extract text from a page with given 'Layout'.
pageTextIO :: Layout -> Page -> IO T.Text
pageTextIO layout (Page _ _ ptr) = withForeignPtr ptr \p -> asText (ffiPageText p l)
  where
    l =
      case layout of
        Raw -> 0
        Physical -> 1
        None -> 2

-- | Extract properties from the document.
-- @since 0.0.2.0
propertiesIO :: Document -> IO Properties
propertiesIO (Document docptr) = withForeignPtr docptr \doc -> do
  a <- asText $ ffiDocumentAuthor doc
  c <- asText $ ffiDocumentCreator doc
  k <- asText $ ffiDocumentKeywords doc
  m <- asText $ ffiDocumentMetadata doc
  p <- asText $ ffiDocumentProducer doc
  s <- asText $ ffiDocumentSubject doc
  t <- asText $ ffiDocumentTitle doc

#ifdef XMLC
  return $ Properties (f a) (f c) (f k) (xml m) (f p) (f s) (f t)
  where
    xml x =
      if T.null x
      then Nothing
      else either (const Nothing) Just $ X.parseText X.def (TL.fromStrict x)
#else
  return $ Properties (f a) (f c) (f k) (f m) (f p) (f s) (f t)
  where
#endif
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
