
module Language.Logic.StdLib where

import Language.Logic.Code
import Language.Logic.Term
import qualified Language.Logic.Eval.Monad as EM

import Polysemy

import Control.Monad
--import Data.Map(Map)
import qualified Data.Map as Map

arg0 :: EvalCtx r => Fact -> Sem r ()
arg0 (Fact _ []) = pure ()
arg0 _ = mzero

arg1 :: EvalCtx r => Fact -> Sem r Term
arg1 (Fact _ [x]) = pure x
arg1 _ = mzero

arg2 :: EvalCtx r => Fact -> Sem r (Term, Term)
arg2 (Fact _ [x, y]) = pure (x, y)
arg2 _ = mzero

builtinToPrim :: forall a. (forall r. EvalCtx r => Fact -> Sem r a) -> (Fact -> EvalEff a)
builtinToPrim g = \fct -> EvalEff $ g fct

writeTerm :: EvalCtx r => Fact -> Sem r ()
writeTerm = arg1 >=> \t -> EM.writeOut (shows t "\n")

stdlib :: CodeBody
stdlib = CodeBody $ Map.fromList [
          ("writeTerm", [
            PrimClause "writeTerm" (builtinToPrim writeTerm)
           ])
         ]
