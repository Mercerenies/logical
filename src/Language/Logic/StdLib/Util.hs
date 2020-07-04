{-# LANGUAGE ConstraintKinds #-}

module Language.Logic.StdLib.Util(EvalCtx',
                                  arg0, arg1, arg2, arg3,
                                  argsForCall, assertCompound, assertString,
                                  builtinToPrim) where

import Language.Logic.Term.Compiled
import Language.Logic.Code
import Language.Logic.Choice
import Language.Logic.Error

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
