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
import Control.Applicative
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

-- Takes at least one argument. The first arg must be a compound term.
-- Any additional arguments are appended to the end of the compound
-- before calling it as a goal.
argsForCall :: EvalCtx' r => Fact -> Sem r Fact
argsForCall (Fact _ []) = mzero -- TODO Is this an error?
argsForCall (Fact _ (TermVar v : _)) = throw (VarNotDone v)
argsForCall (Fact _ (TermCompound f xs : ys)) = pure $ Fact f (xs ++ ys)
argsForCall (Fact _ (t : _)) = throw (TypeError "compound term" t)

builtinToPrim :: forall a. (forall r. EvalCtx' r => Fact -> Sem r a) -> (Fact -> EvalEff a)
builtinToPrim g = \fct -> EvalEff $ nonDetToChoice (g fct)

writeTerm :: EvalCtx' r => Fact -> Sem r ()
writeTerm = arg1 >=> \t -> EM.writeOut (shows t "\n")

fail_ :: EvalCtx' r => Fact -> Sem r ()
fail_ _ = mzero

call :: EvalCtx' r => Fact -> Sem r ()
call = argsForCall >=> evalGoal

once_ :: EvalCtx' r => Fact -> Sem r ()
once_ = argsForCall >=> (once . evalGoal)

not_ :: EvalCtx' r => Fact -> Sem r ()
not_ = argsForCall >=> \fct -> do
         res <- once $ (False <$ evalGoal fct) <|> pure True
         guard res

stdlib :: CodeBody
stdlib = CodeBody $ Map.fromList [
          ("write_term", [
            PrimClause "write_term" (builtinToPrim writeTerm)
           ]),
          ("fail", [
            PrimClause "fail" (builtinToPrim fail_)
           ]),
          ("call", [
            PrimClause "call" (builtinToPrim call)
           ]),
          ("once", [
            PrimClause "once" (builtinToPrim once_)
           ]),
          ("not", [
            PrimClause "not" (builtinToPrim not_)
           ])
         ]

getPrelude :: IO CodeBody
getPrelude = do
  code <- readFile "std/Prelude"
  case tokenizeAndParse "std/Prelude" code of
    Left err -> fail (show err)
    Right clauses -> return $ stdlib <> consolidateClauses clauses
