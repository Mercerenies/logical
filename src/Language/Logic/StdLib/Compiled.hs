{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.StdLib.Compiled(stdlib, getPrelude) where

import Language.Logic.Code
import Language.Logic.Compile
import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Parser
import Language.Logic.Parser.Op(OpTable(..))
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Eval
import Language.Logic.Tagged
import Language.Logic.Unify.Compiled
--import Language.Logic.StdLib.Arithmetic
import Language.Logic.SymbolTable(SymbolTable())
import Language.Logic.SymbolTable.Monad
import qualified Language.Logic.Eval.Monad as EM
import qualified Language.Logic.Util as Util

import Polysemy
import Polysemy.NonDet
import Polysemy.Error

import Control.Monad
import Control.Applicative
--import Data.Map(Map)
import qualified Data.Map as Map

type EvalCtx' r = (EvalCtx r, Member NonDet r)

_arg0 :: EvalCtx' r => CFact -> Sem r ()
_arg0 (CFact _ []) = pure ()
_arg0 _ = mzero

arg1 :: EvalCtx' r => CFact -> Sem r CTerm
arg1 (CFact _ [x]) = pure x
arg1 _ = mzero

_arg2 :: EvalCtx' r => CFact -> Sem r (CTerm, CTerm)
_arg2 (CFact _ [x, y]) = pure (x, y)
_arg2 _ = mzero

arg3 :: EvalCtx' r => CFact -> Sem r (CTerm, CTerm, CTerm)
arg3 (CFact _ [x, y, z]) = pure (x, y, z)
arg3 _ = mzero

-- Takes at least one argument. The first arg must be a compound term.
-- Any additional arguments are appended to the end of the compound
-- before calling it as a goal.
argsForCall :: EvalCtx' r => CFact -> Sem r CFact
argsForCall (CFact _ []) = mzero -- TODO Is this an error?
argsForCall (CFact _ (CTermVar v : _)) = throw (VarsNotDone [v])
argsForCall (CFact _ (CTermCompound f xs : ys)) = pure $ CFact f (xs ++ ys)
argsForCall (CFact _ (t : _)) = throw (TypeError "compound term" (ctermToTerm t))

assertCompound :: EvalCtx' r => CTerm -> Sem r CFact
assertCompound (CTermCompound f xs) = pure $ CFact f xs
assertCompound t = throw (TypeError "compoundCTerm" (ctermToTerm t))

builtinToPrim :: forall a. (forall r. EvalCtx' r => CFact -> Sem r a) -> (CFact -> EvalEff a)
builtinToPrim g = \fct -> EvalEff $ nonDetToChoice (g fct)

writeTerm :: EvalCtx' r => CFact -> Sem r ()
writeTerm = arg1 >=> \t -> EM.writeOut (shows t "\n")

fail_ :: EvalCtx' r => CFact -> Sem r ()
fail_ _ = mzero

call :: EvalCtx' r => CFact -> Sem r ()
call = argsForCall >=> evalGoal

block :: EvalCtx' r => CFact -> Sem r ()
block (CFact _ xs) = mapM assertCompound xs >>= mapM_ evalGoal

-- add takes three arguments. At least two must be ground. If all
-- three are ground, it verifies that the first two sum to the third.
-- If exactly one is a variable, it completes the arithmetic
-- expression. If any terms are not variables or numbers, a type error
-- is raised.
add :: EvalCtx' r => CFact -> Sem r ()
add = arg3 >=> \case
      (CTermNum a, CTermNum b, CTermNum c) -> guard (a + b == c)
      (CTermVar a, CTermNum b, CTermNum c) -> errorToChoice . void $ subAndUnify (CTermVar a) (CTermNum (c - b))
      (CTermNum a, CTermVar b, CTermNum c) -> errorToChoice . void $ subAndUnify (CTermVar b) (CTermNum (c - a))
      (CTermNum a, CTermNum b, CTermVar c) -> errorToChoice . void $ subAndUnify (CTermVar c) (CTermNum (a + b))
      -- Beyond this, there's an error. We just need to decide which error
      (a, b, c)
          | invalid a -> throw (TypeError "variable or number" $ ctermToTerm a)
          | invalid b -> throw (TypeError "variable or number" $ ctermToTerm b)
          | invalid c -> throw (TypeError "variable or number" $ ctermToTerm c)
          | otherwise -> throw (VarsNotDone $ concatMap varOf [a, b, c])
    where invalid (CTermVar _) = False
          invalid (CTermNum _) = False
          invalid _ = True
          varOf (CTermVar v) = [v]
          varOf _ = []

-- mul operates like add in terms of requiring all arguments to be
-- either numbers or variables and requiring at least two ground
-- arguments. Division by zero will simply produce the appropriate
-- floating point infinity or NaN.
mul :: EvalCtx' r => CFact -> Sem r ()
mul = arg3 >=> \case
      (CTermNum a, CTermNum b, CTermNum c) -> guard (a * b == c)
      (CTermVar a, CTermNum b, CTermNum c) -> errorToChoice . void $ subAndUnify (CTermVar a) (CTermNum (c / b))
      (CTermNum a, CTermVar b, CTermNum c) -> errorToChoice . void $ subAndUnify (CTermVar b) (CTermNum (c / a))
      (CTermNum a, CTermNum b, CTermVar c) -> errorToChoice . void $ subAndUnify (CTermVar c) (CTermNum (a * b))
      -- Beyond this, there's an error. We just need to decide which error
      (a, b, c)
          | invalid a -> throw (TypeError "variable or number" $ ctermToTerm a)
          | invalid b -> throw (TypeError "variable or number" $ ctermToTerm b)
          | invalid c -> throw (TypeError "variable or number" $ ctermToTerm c)
          | otherwise -> throw (VarsNotDone $ concatMap varOf [a, b, c])
    where invalid (CTermVar _) = False
          invalid (CTermNum _) = False
          invalid _ = True
          varOf (CTermVar v) = [v]
          varOf _ = []
{-

arithEval :: EvalCtx' r => CFact -> Sem r ()
arithEval = arg2 >=> \(x, t) -> evalArith t >>= errorToChoice . void . subAndUnify x . CTermNum

-- Evaluates the arithmetic expression. If the result is zero, this
-- predicate fails. If not, the predicate succeeds once without
-- unifying anything. The argument must be fully ground.
arithGuard :: EvalCtx' r => CFact -> Sem r ()
arithGuard = arg1 >=> \t -> evalArith t >>= \t' -> if t' == 0 then mzero else pure ()
-} -- ////

-- Evaluates the conditional only once. If the condition succeeds,
-- evaluates the true argument. If it fails, evaluates the false
-- argument. The conditional is only evaluated once, but the other two
-- args are evaluated for all solutions.
if_ :: EvalCtx' r => CFact -> Sem r ()
if_ = arg3 >=> \(c, t, f) -> do
        c' <- assertCompound c
        t' <- assertCompound t
        f' <- assertCompound f
        res <- once $ (True <$ evalGoal c') <|> pure False
        evalGoal (if res then t' else f')

stdlib :: CodeBody String CFact
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
{-
          ("=:", [
            PrimClause "=:" (builtinToPrim arithEval)
           ]),
          ("guard", [
            PrimClause "guard" (builtinToPrim arithGuard)
           ]),
-} -- ////
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

compileLib :: Member (SymbolTableState SymbolId) r =>
              CodeBody String CFact -> Sem r (CodeBody (Tagged Atom SymbolId) CFact)
compileLib (CodeBody m) =
    Util.traverseKeys internTag m >>= traverse (traverse compileClause') >>= return . CodeBody

getPrelude :: SymbolTable -> IO (CodeBody (Tagged Atom SymbolId) CFact, OpTable, SymbolTable)
getPrelude sym = do
  code <- readFile "std/Prelude"
  case tokenizeAndParse (OpTable mempty) sym "std/Prelude" code of
    Left err -> fail (show err)
    Right (clauses, op, sym') ->
        let (sym'', stdlib') = run $ runSymbolTableState sym' (compileLib stdlib)
            (sym''', clauses') = run $ runSymbolTableState sym'' (mapM compileClause clauses) in
        return (stdlib' <> consolidateClauses clauses', op, sym''')
