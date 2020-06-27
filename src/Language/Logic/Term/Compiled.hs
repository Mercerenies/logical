
module Language.Logic.Term.Compiled where

import Language.Logic.Term
import Language.Logic.Tagged
import Language.Logic.Number(Number(..))
import Language.Logic.SymbolTable.Monad

import Polysemy
import qualified Data.Text as T

data CTerm = CTermVar String
           | CTermNum Number
           | CTermCompound (Tagged Atom SymbolId) [CTerm]
             deriving (Eq, Ord)

data CFact = CFact (Tagged Atom SymbolId) [CTerm]
             deriving (Eq, Ord)

ctermToTerm :: CTerm -> Term
ctermToTerm (CTermVar s) = TermVar s
ctermToTerm (CTermNum n) = TermNum n
ctermToTerm (CTermCompound (Tagged (Atom h) _) ts) = TermCompound h $ fmap ctermToTerm ts

cfactToFact :: CFact -> Fact
cfactToFact (CFact (Tagged (Atom h) _) ts) = Fact h $ fmap ctermToTerm ts

instance Show CTerm where
    showsPrec n = showsPrec n . ctermToTerm

instance Show CFact where
    showsPrec n = showsPrec n . cfactToFact

compileTerm :: Member (SymbolTableState SymbolId) r => Term -> Sem r CTerm
compileTerm (TermVar s) = pure (CTermVar s)
compileTerm (TermNum n) = pure (CTermNum n)
compileTerm (TermCompound h ts) = CTermCompound <$> h' <*> ts'
    where h' = Tagged (Atom h) <$> intern (T.pack h)
          ts' = mapM compileTerm ts

compileFact :: Member (SymbolTableState SymbolId) r => Fact -> Sem r CFact
compileFact (Fact h ts) = CFact <$> h' <*> ts'
    where h' = Tagged (Atom h) <$> intern (T.pack h)
          ts' = mapM compileTerm ts
