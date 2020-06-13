
module Language.Logic.Eval.Monad where

import Polysemy

import System.IO

-- A Polysemy effect for the subset of IO that we want to support
-- directly in our interpreter.

data EvalIO m a where
    WriteOut :: String -> EvalIO m ()
    WriteErr :: String -> EvalIO m ()

makeSem ''EvalIO

evalToIO :: Member (Embed IO) r => Sem (EvalIO ': r) a -> Sem r a
evalToIO = interpret $ \case
           WriteOut s -> embed (hPutStr stdout s)
           WriteErr s -> embed (hPutStr stderr s)
