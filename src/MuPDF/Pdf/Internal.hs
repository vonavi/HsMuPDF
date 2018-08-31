{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module MuPDF.Pdf.Internal
  ( CContext
  , CDocument
  , fitzExtraCtx
  ) where

import qualified Data.Map                  as M
import           Data.String               (fromString)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types          as C

data CContext
data CDocument

fitzExtraCtx :: C.Context
fitzExtraCtx = C.baseCtx <> C.bsCtx <> ctx
  where ctx = mempty { C.ctxTypesTable = fitzExtraTypesTable }

fitzExtraTypesTable :: C.TypesTable
fitzExtraTypesTable = M.fromList
  [ (C.TypeName (fromString "fz_context"), [t| CContext |])
  , (C.TypeName (fromString "fz_document"), [t| CDocument |])
  ]
