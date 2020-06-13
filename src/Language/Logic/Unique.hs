
module Language.Logic.Unique(Unique(..), unique, uniques, runUnique, runUniqueInt) where

import Polysemy
import Polysemy.State

import Control.Arrow

data Unique i m a where
    Unique :: Unique i m i

makeSem ''Unique

uniques :: MemberWithError (Unique i) r => (i -> i') -> Sem r i'
uniques f = f <$> unique

runUnique :: i -> (i -> i) -> Sem (Unique i ': r) a -> Sem r a
runUnique x0 next = reinterpret (\Unique -> get >>= \x -> (put $! next x) >> pure x) >>> evalState x0

runUniqueInt :: Integral i => Sem (Unique i ': r) a -> Sem r a
runUniqueInt = runUnique 0 succ
