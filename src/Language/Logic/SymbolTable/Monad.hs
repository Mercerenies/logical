
module Language.Logic.SymbolTable.Monad(SymbolTableState(..), intern,
                                        runSymbolTableState, evalSymbolTableState,
                                        runSymbolTableStateTrivially) where

import Language.Logic.SymbolTable

import Polysemy
import Polysemy.State
import qualified Data.Text as T

data SymbolTableState i m a where
    Intern :: T.Text -> SymbolTableState i m i

makeSem ''SymbolTableState

runSymbolTableStateTrivially :: Sem (SymbolTableState T.Text ': r) a -> Sem r a
runSymbolTableStateTrivially = interpret $ \case
                               Intern s -> pure s

runSymbolTableState :: SymbolTable -> Sem (SymbolTableState Integer ': r) a -> Sem r (SymbolTable, a)
runSymbolTableState table0 = runState table0 . toState

evalSymbolTableState :: SymbolTable -> Sem (SymbolTableState Integer ': r) a -> Sem r a
evalSymbolTableState table0 = evalState table0 . toState

toState :: Sem (SymbolTableState Integer ': r) a -> Sem (State SymbolTable ': r) a
toState = reinterpret $ \case
          Intern t -> do
            table <- get
            let (i, table') = internInTable table t
            put $! table'
            return i
