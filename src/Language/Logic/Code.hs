{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.Code where

import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Unify(AssumptionState)
import Language.Logic.Unique
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Tagged
import qualified Language.Logic.Util as Util
import qualified Language.Logic.Eval.Monad as EM
import qualified Language.Logic.SymbolTable.Monad as SM

import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Data.Map(Map)
import qualified Data.Map as Map

type EvalCtx r = (Member (Reader (CodeBody Fact)) r, Member Choice r, Member (Unique Int) r,
                  Member AssumptionState r, Member EM.EvalIO r, Member (Error RuntimeError) r,
                  Member (SM.SymbolTableState SM.SymbolId) r)

data EvalEff a = EvalEff (forall r. EvalCtx r => Sem r a)

data Clause f = StdClause f [f]
              | PrimClause String (f -> EvalEff ())

-- It would be slightly more efficient to index the map by both name
-- and arity (as opposed to just name), but that's probably
-- insignificant.
data CodeBody f = CodeBody (Map String [Clause f])

instance Semigroup (CodeBody f) where
    CodeBody a <> CodeBody b = CodeBody $ Map.unionWith (++) a b

instance Monoid (CodeBody f) where
    mempty = CodeBody mempty

instance Show f => Show (Clause f) where
    showsPrec _ (StdClause h []) = shows h . ("." ++)
    showsPrec _ (StdClause h ts) = shows h . (": " ++) . Util.sepBy (" " ++) (fmap shows ts)
    showsPrec _ (PrimClause s _) = (s ++) . (": (primitive)" ++)

instance Show f => Show (CodeBody f) where
    showsPrec _ (CodeBody m) =
        let clauses = map snd $ Map.toAscList m
            str = fmap (\cs -> foldr (.) id (fmap (\c -> shows c . ("\n" ++)) cs) . ("\n" ++)) clauses
        in foldr (.) id str

class IsFact f where
    type FactHead f
    type FactTerm f
    factHead :: f -> FactHead f
    factBody :: f -> [FactTerm f]

instance IsFact Fact where
    type FactHead Fact = String
    type FactTerm Fact = Term
    factHead (Fact h _) = h
    factBody (Fact _ ts) = ts

instance IsFact CFact where
    type FactHead CFact = Tagged Atom SM.SymbolId
    type FactTerm CFact = CTerm
    factHead (CFact h _) = h
    factBody (CFact _ ts) = ts

lookupHead :: String -> CodeBody f -> [Clause f]
lookupHead s (CodeBody m) = maybe [] id $ Map.lookup s m

consolidateClauses :: [Clause Fact] -> CodeBody Fact
consolidateClauses = CodeBody . Util.classify clauseHead
    where clauseHead (StdClause (Fact h _) _) = h
          clauseHead (PrimClause s _) = s

runEvalEff :: EvalCtx r => EvalEff a -> Sem r a
runEvalEff (EvalEff a) = a
