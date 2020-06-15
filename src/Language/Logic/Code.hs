{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.Code where

import Language.Logic.Term
import Language.Logic.Unify(AssumptionState)
import Language.Logic.Unique
import Language.Logic.Choice
import Language.Logic.Error
import qualified Language.Logic.Util as Util
import qualified Language.Logic.Eval.Monad as EM

import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Data.Map(Map)
import qualified Data.Map as Map

type EvalCtx r = (Member (Reader CodeBody) r, Member Choice r, Member (Unique Int) r,
                  Member AssumptionState r, Member EM.EvalIO r, Member (Error RuntimeError) r)

data EvalEff a = EvalEff (forall r. EvalCtx r => Sem r a)

data Clause = StdClause Fact [Fact]
            | PrimClause String (Fact -> EvalEff ())

-- It would be slightly more efficient to index the map by both name
-- and arity (as opposed to just name), but that's probably
-- insignificant.
data CodeBody = CodeBody (Map String [Clause])

instance Semigroup CodeBody where
    CodeBody a <> CodeBody b = CodeBody $ Map.unionWith (++) a b

instance Monoid CodeBody where
    mempty = CodeBody mempty

instance Show Clause where
    showsPrec _ (StdClause h ts) = shows h . (": " ++) . Util.sepBy (" " ++) (fmap shows ts)
    showsPrec _ (PrimClause s _) = (s ++) . (": (primitive)" ++)

lookupHead :: String -> CodeBody -> [Clause]
lookupHead s (CodeBody m) = maybe [] id $ Map.lookup s m

consolidateClauses :: [Clause] -> CodeBody
consolidateClauses = CodeBody . Util.classify clauseHead
    where clauseHead (StdClause (Fact h _) _) = h
          clauseHead (PrimClause s _) = s

runEvalEff :: EvalCtx r => EvalEff a -> Sem r a
runEvalEff (EvalEff a) = a
