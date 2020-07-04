
module Language.Logic.SymbolTable.Monad(SymbolTableState(..), SymbolId, intern, lookup,
                                        runSymbolTableState, evalSymbolTableState,
                                        runSymbolTableStateTrivially) where

import Language.Logic.SymbolTable(SymbolTable, SymbolId)
import qualified Language.Logic.SymbolTable as SymbolTable

import Polysemy
import Polysemy.State
import qualified Data.Text as T

import Prelude hiding (lookup)

data SymbolTableState i m a where
    Intern :: T.Text -> SymbolTableState i m i
    Lookup :: i -> SymbolTableState i m (Maybe T.Text)

makeSem ''SymbolTableState

runSymbolTableStateTrivially :: Sem (SymbolTableState T.Text ': r) a -> Sem r a
runSymbolTableStateTrivially = interpret $ \case
                               Intern s -> pure s
                               Lookup s -> pure (Just s)

runSymbolTableState :: SymbolTable -> Sem (SymbolTableState SymbolId ': r) a -> Sem r (SymbolTable, a)
runSymbolTableState table0 = runLazyState table0 . toState

evalSymbolTableState :: SymbolTable -> Sem (SymbolTableState SymbolId ': r) a -> Sem r a
evalSymbolTableState table0 = evalLazyState table0 . toState

toState :: Sem (SymbolTableState SymbolId ': r) a -> Sem (State SymbolTable ': r) a
toState = reinterpret $ \case
          Intern t -> do
            table <- get
            let (i, table') = SymbolTable.intern t table
            put $! table'
            return i
          Lookup i -> gets (SymbolTable.lookup i)

-- ///// atom_string which uses intern and lookup internally
