{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.Code where

import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Unique
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Tagged
import Language.Logic.VMData
import Language.Logic.Debug
import qualified Language.Logic.Util as Util
import qualified Language.Logic.Eval.Monad as EM
import qualified Language.Logic.SymbolTable.Monad as SM
import qualified Language.Logic.Unify.Compiled as UC

import Polysemy
import Polysemy.Reader
import Polysemy.Error
import Colog.Polysemy(Log)

import Data.Map(Map)
import qualified Data.Map as Map

type EvalCtx r = (Member (Reader (CodeBody (Tagged Atom SM.SymbolId) CFact)) r, Member Choice r,
                  Member (Unique Int) r, Member VMEnv r, Member (Log Message) r,
                  Member UC.AssumptionState r, Member EM.EvalIO r, Member (Error RuntimeError) r,
                  Member (SM.SymbolTableState SM.SymbolId) r)

data EvalEff a = EvalEff (forall r. EvalCtx r => Sem r a)

data Clause k f = StdClause f [f]
                | PrimClause k (f -> EvalEff ())

-- It would be slightly more efficient to index the map by both name
-- and arity (as opposed to just name), but that's probably
-- insignificant.
data CodeBody k f = CodeBody (Map k [Clause k f])

instance Ord k => Semigroup (CodeBody k f) where
    CodeBody a <> CodeBody b = CodeBody $ Map.unionWith (++) a b

instance Ord k => Monoid (CodeBody k f) where
    mempty = CodeBody mempty

instance (Show k, Show f) => Show (Clause k f) where
    showsPrec _ (StdClause h []) = shows h . ("." ++)
    showsPrec _ (StdClause h ts) = shows h . (": " ++) . Util.sepBy (" " ++) (fmap shows ts)
    showsPrec _ (PrimClause s _) = shows s . (": (primitive)" ++)

instance (Ord k, Show k, Show f) => Show (CodeBody k f) where
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

lookupHead :: Ord k => k -> CodeBody k f -> [Clause k f]
lookupHead s (CodeBody m) = maybe [] id $ Map.lookup s m

consolidateClauses :: (IsFact f, Ord (FactHead f)) => [Clause (FactHead f) f] -> CodeBody (FactHead f) f
consolidateClauses = CodeBody . Util.classify clauseHead
    where clauseHead (StdClause f _) = factHead f
          clauseHead (PrimClause s _) = s

runEvalEff :: EvalCtx r => EvalEff a -> Sem r a
runEvalEff (EvalEff a) = a
