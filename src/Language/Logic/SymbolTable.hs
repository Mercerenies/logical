
module Language.Logic.SymbolTable(SymbolTable(), SymbolId, emptyTable, intern, lookup) where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Prelude hiding (lookup)

data SymbolTable = SymbolTable {
      _stTable :: !(HashMap T.Text SymbolId),
      _stRevTable :: !(HashMap SymbolId T.Text),
      _stNextIndex :: !SymbolId
    }

type SymbolId = Integer

emptyTable :: SymbolTable
emptyTable = SymbolTable HashMap.empty HashMap.empty 0

-- If using Polysemy to interface with a symbol table, then the intern
-- function provided in Language.Logic.SymbolTable.Monad should be
-- used instead. This one provides the underlying functionality and is
-- exposed to be used in cases where we want to modify the symbol
-- table directly or need to use the interface with a traditional
-- mtl-style MonadState instance. In the latter case, this function
-- can be used as `state (intern text)`.
intern :: T.Text -> SymbolTable -> (SymbolId, SymbolTable)
intern t (SymbolTable m m' imax) =
    case HashMap.lookup t m of
      Just i -> (i, SymbolTable m m' imax)
      Nothing -> (imax, SymbolTable (HashMap.insert t imax m) (HashMap.insert imax t m') (imax + 1))

lookup :: SymbolId -> SymbolTable -> Maybe T.Text
lookup i (SymbolTable _ m' _) = HashMap.lookup i m'
