{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.StdLib where

import Language.Logic.Code
import Language.Logic.Term
import Language.Logic.Parser
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Eval
import Language.Logic.Unify
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

arg3 :: EvalCtx' r => Fact -> Sem r (Term, Term, Term)
arg3 (Fact _ [x, y, z]) = pure (x, y, z)
arg3 _ = mzero

-- Takes at least one argument. The first arg must be a compound term.
-- Any additional arguments are appended to the end of the compound
-- before calling it as a goal.
argsForCall :: EvalCtx' r => Fact -> Sem r Fact
argsForCall (Fact _ []) = mzero -- TODO Is this an error?
argsForCall (Fact _ (TermVar v : _)) = throw (VarsNotDone [v])
argsForCall (Fact _ (TermCompound f xs : ys)) = pure $ Fact f (xs ++ ys)
argsForCall (Fact _ (t : _)) = throw (TypeError "compound term" t)

assertCompound :: EvalCtx' r => Term -> Sem r Fact
assertCompound (TermCompound f xs) = pure $ Fact f xs
assertCompound t = throw (TypeError "compoundTerm" t)

builtinToPrim :: forall a. (forall r. EvalCtx' r => Fact -> Sem r a) -> (Fact -> EvalEff a)
builtinToPrim g = \fct -> EvalEff $ nonDetToChoice (g fct)

writeTerm :: EvalCtx' r => Fact -> Sem r ()
writeTerm = arg1 >=> \t -> EM.writeOut (shows t "\n")

fail_ :: EvalCtx' r => Fact -> Sem r ()
fail_ _ = mzero

call :: EvalCtx' r => Fact -> Sem r ()
call = argsForCall >=> evalGoal

block :: EvalCtx' r => Fact -> Sem r ()
block (Fact _ xs) = mapM assertCompound xs >>= mapM_ evalGoal

-- add takes three arguments. At least two must be ground. If all
-- three are ground, it verifies that the first two sum to the third.
-- If exactly one is a variable, it completes the arithmetic
-- expression. If any terms are not variables or numbers, a type error
-- is raised.
add :: EvalCtx' r => Fact -> Sem r ()
add = arg3 >=> \case
      (TermNum a, TermNum b, TermNum c) -> guard (a + b == c)
      (TermVar a, TermNum b, TermNum c) -> errorToChoice . void $ subAndUnify (TermVar a) (TermNum (c - b))
      (TermNum a, TermVar b, TermNum c) -> errorToChoice . void $ subAndUnify (TermVar b) (TermNum (c - a))
      (TermNum a, TermNum b, TermVar c) -> errorToChoice . void $ subAndUnify (TermVar c) (TermNum (a + b))
      -- Beyond this, there's an error. We just need to decide which error
      (a, b, c)
          | invalid a -> throw (TypeError "variable or number" a)
          | invalid b -> throw (TypeError "variable or number" b)
          | invalid c -> throw (TypeError "variable or number" c)
          | otherwise -> throw (VarsNotDone $ concatMap varOf [a, b, c])
    where invalid (TermVar _) = False
          invalid (TermNum _) = False
          invalid _ = True
          varOf (TermVar v) = [v]
          varOf _ = []

-- mul operates like add in terms of requiring all arguments to be
-- either numbers or variables and requiring at least two ground
-- arguments. Division by zero will simply produce the appropriate
-- floating point infinity or NaN.
mul :: EvalCtx' r => Fact -> Sem r ()
mul = arg3 >=> \case
      (TermNum a, TermNum b, TermNum c) -> guard (a * b == c)
      (TermVar a, TermNum b, TermNum c) -> errorToChoice . void $ subAndUnify (TermVar a) (TermNum (c / b))
      (TermNum a, TermVar b, TermNum c) -> errorToChoice . void $ subAndUnify (TermVar b) (TermNum (c / a))
      (TermNum a, TermNum b, TermVar c) -> errorToChoice . void $ subAndUnify (TermVar c) (TermNum (a * b))
      -- Beyond this, there's an error. We just need to decide which error
      (a, b, c)
          | invalid a -> throw (TypeError "variable or number" a)
          | invalid b -> throw (TypeError "variable or number" b)
          | invalid c -> throw (TypeError "variable or number" c)
          | otherwise -> throw (VarsNotDone $ concatMap varOf [a, b, c])
    where invalid (TermVar _) = False
          invalid (TermNum _) = False
          invalid _ = True
          varOf (TermVar v) = [v]
          varOf _ = []

-- Evaluates the conditional only once. If the condition succeeds,
-- evaluates the true argument. If it fails, evaluates the false
-- argument. The conditional is only evaluated once, but the other two
-- args are evaluated for all solutions.
if_ :: EvalCtx' r => Fact -> Sem r ()
if_ = arg3 >=> \(c, t, f) -> do
        c' <- assertCompound c
        t' <- assertCompound t
        f' <- assertCompound f
        res <- once $ (True <$ evalGoal c') <|> pure False
        if res then evalGoal t' else evalGoal f'

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
          ("if", [
            PrimClause "if" (builtinToPrim if_)
           ]),
          ("block", [
            PrimClause "block" (builtinToPrim block)
           ]),
          ("add", [
            PrimClause "add" (builtinToPrim add)
           ]),
          ("mul", [
            PrimClause "mul" (builtinToPrim mul)
           ])
         ]

getPrelude :: IO CodeBody
getPrelude = do
  code <- readFile "std/Prelude"
  case tokenizeAndParse "std/Prelude" code of
    Left err -> fail (show err)
    Right clauses -> return $ stdlib <> consolidateClauses clauses
