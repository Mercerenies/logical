
module Language.Logic.SymbolTable(SymbolTable(), SymbolId, emptyTable, internInTable) where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

data SymbolTable = SymbolTable {
      _stTable :: !(HashMap T.Text SymbolId),
      _stNextIndex :: !SymbolId
    }

type SymbolId = Integer

emptyTable :: SymbolTable
emptyTable = SymbolTable HashMap.empty 0

internInTable :: SymbolTable -> T.Text -> (SymbolId, SymbolTable)
internInTable (SymbolTable m imax) t =
    case HashMap.lookup t m of
      Just i -> (i, SymbolTable m imax)
      Nothing -> (imax, SymbolTable (HashMap.insert t imax m) (imax + 1))
