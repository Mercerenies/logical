
module Language.Logic.VMData where

import Language.Logic.StdLib.Arithmetic

import Polysemy
import Polysemy.Reader

-- Some special VM-specific data gets compiled and put here, so it can
-- be accessed by any primitive predicates that need it. The user of
-- the language should never need to interface directly with this.

data VMEnv m a where
    GetArithmetic :: VMEnv m ArithFnsC

data VMData = VMData {
      vmArithFns :: ArithFnsC
    }

makeSem ''VMEnv

runVMEnv :: forall r a. VMData -> Sem (VMEnv ': r) a -> Sem r a
runVMEnv dat = runReader dat . reinterpret go
    where go :: forall m x. VMEnv m x -> Sem (Reader VMData ': r) x
          go GetArithmetic = asks vmArithFns
