{-# LANGUAGE ViewPatterns #-}

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

-- Note: This is currently just a slower CTermVar. At one point, it
-- served the purpose of capturing variable-like identifiers (when _
-- wasn't technically a variable) as well.
pattern CTermIsVar :: String -> CTerm
pattern CTermIsVar v <- (varNameC -> Just v)
    where CTermIsVar v = CTermVar v

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

freeVarsC :: CTerm -> [String]
freeVarsC (CTermVar s) = [s]
freeVarsC (CTermNum {}) = []
freeVarsC (CTermCompound _ ts) = concatMap freeVarsC ts

freeVarsInCFact :: CFact -> [String]
freeVarsInCFact (CFact _ xs) = concatMap freeVarsC xs

traverseVarsC :: Applicative f => (String -> f CTerm) -> CTerm -> f CTerm
traverseVarsC f = go
    where go (CTermVar s) = f s
          go (CTermNum n) = pure (CTermNum n)
          go (CTermCompound s args) = CTermCompound s <$> traverse go args

traverseVarsInCFact :: Applicative f => (String -> f CTerm) -> CFact -> f CFact
traverseVarsInCFact f (CFact h ts) = CFact h <$> traverse (traverseVarsC f) ts

varNameC :: CTerm -> Maybe String
varNameC (CTermVar s) = Just s
varNameC _ = Nothing
