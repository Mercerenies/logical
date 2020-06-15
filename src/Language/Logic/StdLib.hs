{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.StdLib where

import Language.Logic.Code
import Language.Logic.Term
import Language.Logic.Parser
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Eval
import qualified Language.Logic.Eval.Monad as EM

import Polysemy
import Polysemy.NonDet
import Polysemy.Error

import Control.Monad
--import Data.Map(Map)
import qualified Data.Map as Map

type EvalCtx' r = (EvalCtx r, Member NonDet r)

arg0 :: EvalCtx' r => Fact -> Sem r ()
arg0 (Fact _ []) = pure ()
arg0 _ = mzero

arg1 :: EvalCtx' r => Fact -> Sem r Term
arg1 (Fact _ [x]) = pure x
arg1 _ = mzero

arg2 :: EvalCtx' r => Fact -> Sem r (Term, Term)
arg2 (Fact _ [x, y]) = pure (x, y)
arg2 _ = mzero

builtinToPrim :: forall a. (forall r. EvalCtx' r => Fact -> Sem r a) -> (Fact -> EvalEff a)
builtinToPrim g = \fct -> EvalEff $ nonDetToChoice (g fct)

writeTerm :: EvalCtx' r => Fact -> Sem r ()
writeTerm = arg1 >=> \t -> EM.writeOut (shows t "\n")

fail_ :: EvalCtx' r => Fact -> Sem r ()
fail_ _ = mzero

-- Takes at least one argument. The first arg must be a compound term.
-- Any additional arguments are appended to the end of the compound
-- before calling it as a goal.
call :: EvalCtx' r => Fact -> Sem r ()
call fct = case fct of
             Fact _ [] -> mzero -- TODO Is this an error?
             Fact _ (TermVar v : _) -> throw (VarNotDone v)
             Fact _ (TermCompound f xs : ys) -> evalGoal (Fact f (xs ++ ys))
             Fact _ (t : _) -> throw (TypeError "compound term" t)

stdlib :: CodeBody
stdlib = CodeBody $ Map.fromList [
          ("writeTerm", [
            PrimClause "writeTerm" (builtinToPrim writeTerm)
           ]),
          ("fail", [
            PrimClause "fail" (builtinToPrim fail_)
           ]),
          ("call", [
            PrimClause "call" (builtinToPrim call)
           ])
         ]

getPrelude :: IO CodeBody
getPrelude = do
  code <- readFile "std/Prelude"
  case tokenizeAndParse "std/Prelude" code of
    Left err -> fail (show err)
    Right clauses -> return $ stdlib <> consolidateClauses clauses
