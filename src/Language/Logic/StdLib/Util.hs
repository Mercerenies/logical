{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.StdLib.Util(EvalCtx',
                                  arg0, arg1, arg2, arg3,
                                  argsForCall, assertCompound, assertString,
                                  builtinToPrim, unify,
                                  expectingError) where

import Language.Logic.Term.Compiled
import Language.Logic.Unify.Compiled
import Language.Logic.Code
import Language.Logic.Choice
import Language.Logic.Error
import Language.Logic.Eval

import Polysemy
import Polysemy.NonDet
import Polysemy.Error
import qualified Data.Text as T

import Control.Monad

type EvalCtx' r = (EvalCtx r, Member NonDet r)

arg0 :: EvalCtx' r => CFact -> Sem r ()
arg0 (CFact _ []) = pure ()
arg0 _ = mzero

arg1 :: EvalCtx' r => CFact -> Sem r CTerm
arg1 (CFact _ [x]) = pure x
arg1 _ = mzero

arg2 :: EvalCtx' r => CFact -> Sem r (CTerm, CTerm)
arg2 (CFact _ [x, y]) = pure (x, y)
arg2 _ = mzero

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
assertCompound t = throw (TypeError "compound term" (ctermToTerm t))

assertString :: EvalCtx' r => CTerm -> Sem r T.Text
assertString (CTermString t) = pure t
assertString term = throw (TypeError "string" (ctermToTerm term))

builtinToPrim :: forall a. (forall r. EvalCtx' r => CFact -> Sem r a) -> (CFact -> EvalEff a)
builtinToPrim g = \fct -> EvalEff $ nonDetToChoice (g fct)

unify :: (Member AssumptionState r, Member NonDet r) => CTerm -> CTerm -> Sem r ()
unify a b = errorToNonDet . void $ subAndUnify a b

-- This always throws a runtime error. It determines which runtime
-- error to throw as follows.
--
-- 1. If any of the terms fail to satisfy the valid predicate, then
--    the error will be a type error referencing the first term which
--    fails.
-- 2. If all terms satisfy the valid predicate, then a vars-not-done
--    error will be thrown, where all terms which are variables are
--    included in the error list.
expectingError :: Member (Error RuntimeError) r => (CTerm -> Bool) -> String -> [CTerm] -> Sem r b
expectingError valid expecting terms = do
  forM_ terms $ \t -> unless (valid t) $ throw (TypeError expecting (ctermToTerm t))
  throw (VarsNotDone $ concatMap varOf terms)
      where varOf (CTermIsVar v) = [v]
            varOf _ = []
