{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module MuPDF.Pdf.Document
  ( Document
  , openDocument
  , dropDocument
  , withDocument
  , countPages
  ) where

import           Control.Exception     (bracket)
import qualified Data.ByteString.Char8 as BC
import           Foreign.Ptr           (Ptr)
import qualified Language.C.Inline     as C

import           MuPDF.Pdf.Internal    (CContext, CDocument, fitzExtraCtx)

C.context fitzExtraCtx

C.include "<mupdf/fitz.h>"

data Document = Document (Ptr CContext) (Ptr CDocument)

openDocument :: FilePath -> IO Document
openDocument filename = do
  ctxPtr <- [C.exp| fz_context* {
                fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED)
            } |]
  let
    bs = BC.pack filename
  Document ctxPtr <$>
    [C.block| fz_document* {
        fz_register_document_handlers($(fz_context* ctxPtr));
        fz_open_document($(fz_context* ctxPtr), $bs-ptr:bs);
    } |]

dropDocument :: Document -> IO ()
dropDocument (Document ctxPtr docPtr) =
  [C.block| void {
      fz_drop_document($(fz_context* ctxPtr), $(fz_document* docPtr));
      fz_drop_context($(fz_context* ctxPtr));
  } |]

withDocument :: FilePath -> (Document -> IO r) -> IO r
withDocument filename = bracket (openDocument filename) dropDocument

countPages :: Document -> IO C.CInt
countPages (Document ctxPtr docPtr) =
  [C.exp| int {
      fz_count_pages($(fz_context* ctxPtr), $(fz_document* docPtr))
  } |]
