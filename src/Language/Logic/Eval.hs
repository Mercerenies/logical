
module Language.Logic.Eval where

import Language.Logic.Code
import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Unify
import Language.Logic.VMData
import Language.Logic.Util
import Language.Logic.Unique
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Tagged
import Language.Logic.Compile
import Language.Logic.SymbolTable(SymbolTable())
import qualified Language.Logic.Eval.Monad as EM
import qualified Language.Logic.SymbolTable.Monad as SM
import qualified Language.Logic.Unify.Compiled as UC

import Polysemy
import Polysemy.Reader
import Polysemy.NonDet
import Polysemy.Error

import Control.Monad
import Data.Function
import Data.Bifunctor
import qualified Data.Map as Map

--import Debug.Trace

type CClause = Clause (Tagged Atom SM.SymbolId) CFact

errorToNonDet :: forall e a r. Member NonDet r => Sem (Error e ': r) a -> Sem r a
errorToNonDet m = runError m >>= either (const mzero) pure

errorToChoice :: forall e a r. Member Choice r => Sem (Error e ': r) a -> Sem r a
errorToChoice = nonDetToChoice . errorToNonDet . raiseUnder

freshVar :: Member (Unique Int) r => Sem r String
freshVar = uniques (\n -> "_G" ++ show n)

replaceUnderscore :: Member (Unique Int) r => String -> Sem r String
replaceUnderscore "_" = freshVar
replaceUnderscore x   = pure x

freshenClause :: Member (Unique Int) r => Clause String Fact -> Sem r (Clause String Fact)
freshenClause (StdClause concl inner) = do
  concl' <- traverseVarsInFact (fmap TermVar . replaceUnderscore) concl
  inner' <- traverse (traverseVarsInFact (fmap TermVar . replaceUnderscore)) inner
  let vars = freeVarsInFact concl' ++ concatMap freeVarsInFact inner'
  freshVars <- replicateM (length vars) freshVar
  let asm = Assumptions $ Map.fromList (zip vars $ fmap TermVar freshVars)
      concl'' = subOnceInFact asm concl'
      inner'' = fmap (subOnceInFact asm) inner'
  return $ StdClause concl'' inner''
freshenClause (PrimClause s p) = pure $ PrimClause s p

freshenClauseC :: Member (Unique Int) r => CClause -> Sem r CClause
freshenClauseC (StdClause concl inner) = do
  concl' <- traverseVarsInCFact (fmap CTermVar . replaceUnderscore) concl
  inner' <- traverse (traverseVarsInCFact (fmap CTermVar . replaceUnderscore)) inner
  let vars = freeVarsInCFact concl' ++ concatMap freeVarsInCFact inner'
  freshVars <- replicateM (length vars) freshVar
  let asm = UC.Assumptions $ Map.fromList (zip vars $ fmap CTermVar freshVars)
      concl'' = UC.subOnceInFact asm concl'
      inner'' = fmap (UC.subOnceInFact asm) inner'
  return $ StdClause concl'' inner''
freshenClauseC (PrimClause s p) = pure $ PrimClause s p

matchClause0 :: EvalCtx r => CFact -> CClause -> Sem r ()
matchClause0 fact (StdClause concl inner) = do
  fact' <- UC.doSubFact fact
  concl' <- UC.doSubFact concl
  inner' <- mapM UC.doSubFact inner
  nonDetToChoice . guard $ factHead fact' == factHead concl'
  nonDetToChoice . guard $ length (factBody fact') == length (factBody concl')
  errorToChoice $ zipWithM_ UC.subAndUnify (factBody fact') (factBody concl')
  mapM_ (UC.doSubFact >=> evalGoal) inner'
matchClause0 fact (PrimClause s p) = do
  fact' <- UC.doSubFact fact
  nonDetToChoice . guard $ factHead fact' == s
  runEvalEff $ p fact'

matchClause :: EvalCtx r => CFact -> CClause -> Sem r ()
matchClause fact clause = freshenClauseC clause >>= matchClause0 fact

evalGoal :: EvalCtx r => CFact -> Sem r ()
evalGoal fact = do
  --traceM $ "GOAL  " ++ show fact
  clauses <- asks (lookupHead $ factHead fact)
  nonDetToChoice $ oneOf (fmap (matchClause fact) clauses)

getMainGoal :: EvalCtx r => Sem r CFact
getMainGoal = internTag "main" >>= \main -> pure $ CFact main []

runProgram :: VMData -> SymbolTable -> CodeBody (Tagged Atom SM.SymbolId) CFact ->
              IO (Either RuntimeError (SymbolTable, Int))
runProgram vm sym body =
    (getMainGoal >>= evalGoal) &
    runReader body &
    runVMEnv vm &
    UC.evalAssumptionState &
    errorToChoice @UnifyError &
    runChoice @[] &
    runUniqueInt & -- TODO Swap this with the above line? Is it safe?
                   -- It would make the var numbers less insane.
    SM.runSymbolTableState sym &
    EM.evalToIO &
    runError @RuntimeError &
    embedToFinal &
    runFinal &
    fmap (second (second length))

