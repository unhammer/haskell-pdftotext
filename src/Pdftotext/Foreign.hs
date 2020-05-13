{-# LANGUAGE BlockArguments #-}

{- ORMOLU_DISABLE -}
{-|
Module      : Pdftotext.Foreign
Description : Foreign interface
Copyright   : (c) 2020 G. Eyaeb
License     : BSD-3-Clause
Maintainer  : geyaeb@protonmail.com
Stability   : experimental
Portability : POSIX
-}
{- ORMOLU_ENABLE -}
module Pdftotext.Foreign
  ( -- * C++ objects
    Poppler_Document,
    Poppler_Page,

    -- * 'std::string' helper
    StdString,
    asText,
    stringToText,

    -- * FFI
    ffiOpenPdf,
    ffiOpenData,
    ffiDocumentDelete,
    ffiDocumentPages,
    ffiDocumentOpenPage,
    ffiPageDelete,
    ffiPageText,
    ffiStringLength,
    ffiStringDelete,
    ffiStringCopy,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign
import Foreign.C

data Poppler_Document

data Poppler_Page

data CStdString

type StdString = Ptr CStdString

foreign import ccall unsafe "poppler_document_open_pdf"
  ffiOpenPdf :: CString -> IO (Ptr Poppler_Document)

foreign import ccall unsafe "poppler_document_open_data"
  ffiOpenData :: Ptr Word8 -> CInt -> IO (Ptr Poppler_Document)

foreign import ccall unsafe "&poppler_document_delete"
  ffiDocumentDelete :: FunPtr (Ptr Poppler_Document -> IO ())

foreign import ccall unsafe "poppler_document_pages"
  ffiDocumentPages :: Ptr Poppler_Document -> IO CInt

foreign import ccall unsafe "poppler_document_open_page"
  ffiDocumentOpenPage :: Ptr Poppler_Document -> CInt -> IO (Ptr Poppler_Page)

foreign import ccall unsafe "&poppler_page_delete"
  ffiPageDelete :: FunPtr (Ptr Poppler_Page -> IO ())

foreign import ccall unsafe "poppler_page_text"
  ffiPageText :: Ptr Poppler_Page -> CBool -> IO StdString

foreign import ccall unsafe "string_get_length"
  ffiStringLength :: StdString -> IO CUInt

foreign import ccall unsafe "string_delete"
  ffiStringDelete :: StdString -> IO ()

foreign import ccall unsafe "string_copy"
  ffiStringCopy :: StdString -> Ptr CChar -> IO ()

-- | Converts `std::string` wrapped in IO into `Data.Text`.
asText :: IO StdString -> IO T.Text
asText = (>>= stringToText)

-- | Converts `std::string` into `Data.Text`.
stringToText :: StdString -> IO T.Text
stringToText ptr = do
  len <- fromIntegral <$> ffiStringLength ptr
  allocaBytes len \out ->
    ffiStringCopy ptr out
      >> T.peekCStringLen (out, len)
      <* ffiStringDelete ptr
