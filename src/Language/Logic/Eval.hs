
module Language.Logic.Eval where

import Language.Logic.Code
import Language.Logic.Term
import Language.Logic.Unify
import Language.Logic.Util
import Language.Logic.Unique
import qualified Language.Logic.Eval.Monad as EM

import Polysemy
import Polysemy.Reader
import Polysemy.NonDet
import Polysemy.Error

import Control.Monad
import Data.Function
import qualified Data.Map as Map

--import Debug.Trace

errorToNonDet :: Member NonDet r => Sem (Error e ': r) a -> Sem r a
errorToNonDet m = runError m >>= either (const mzero) pure

freshVar :: Member (Unique Int) r => Sem r String
freshVar = uniques (\n -> "_G" ++ show n)

freshenClause :: Member (Unique Int) r => Clause -> Sem r Clause
freshenClause (StdClause concl inner) = do
  let vars = freeVarsInFact concl ++ concatMap freeVarsInFact inner
  freshVars <- replicateM (length vars) freshVar
  let asm = Assumptions $ Map.fromList (zip vars $ fmap TermVar freshVars)
      concl' = subOnceInFact asm concl
      inner' = fmap (subOnceInFact asm) inner
  return $ StdClause concl' inner'
freshenClause (PrimClause s p) = pure $ PrimClause s p

matchClause0 :: (Member (Reader CodeBody) r, Member NonDet r, Member (Unique Int) r, Member AssumptionState r,
                 Member EM.EvalIO r) =>
                Fact -> Clause -> Sem r ()
matchClause0 fact (StdClause concl inner) = do
  fact' <- doSubFact fact
  concl' <- doSubFact concl
  inner' <- mapM doSubFact inner
  guard $ factHead fact' == factHead concl'
  guard $ length (factBody fact') == length (factBody concl')
  errorToNonDet $ zipWithM_ subAndUnify (factBody fact') (factBody concl')
  mapM_ (doSubFact >=> evalGoal) inner'
matchClause0 fact (PrimClause s p) = do
  fact' <- doSubFact fact
  guard $ factHead fact' == s
  runEvalEff $ p fact'

matchClause :: (Member (Reader CodeBody) r, Member NonDet r, Member (Unique Int) r, Member AssumptionState r,
                Member EM.EvalIO r) =>
               Fact -> Clause -> Sem r ()
matchClause fact clause = freshenClause clause >>= matchClause0 fact

evalGoal :: (Member (Reader CodeBody) r, Member NonDet r, Member (Unique Int) r, Member AssumptionState r,
             Member EM.EvalIO r) =>
            Fact -> Sem r ()
evalGoal fact = do
  --traceM $ "GOAL  " ++ show fact
  clauses <- asks (lookupHead $ factHead fact)
  clause <- oneOf clauses
  matchClause fact clause

runProgram :: CodeBody -> IO ()
runProgram body = evalGoal (Fact "main" []) &
                  runReader body &
                  evalAssumptionState &
                  errorToNonDet &
                  runNonDet @[] &
                  runUniqueInt & -- TODO Swap this with the above
                                 -- line? Is it safe? It would make
                                 -- the var numbers less insane.
                  EM.evalToIO &
                  embedToFinal &
                  runFinal &
                  void

