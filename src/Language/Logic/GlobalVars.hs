
module Language.Logic.GlobalVars where

import Language.Logic.Unique

import Polysemy
import Polysemy.State
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable

import Data.Maybe(fromJust)

data GlobalVars k v m a where
    DefineGlobal :: v -> GlobalVars k v m k
    GetGlobal :: k -> GlobalVars k v m v
    SetGlobal :: k -> v -> GlobalVars k v m ()

makeSem ''GlobalVars

modifyGlobal :: MemberWithError (GlobalVars k v) r => k -> (v -> v) -> Sem r ()
modifyGlobal k f = getGlobal k >>= \v -> setGlobal k $ f v

modifyGlobal' :: MemberWithError (GlobalVars k v) r => k -> (v -> v) -> Sem r ()
modifyGlobal' k f = getGlobal k >>= \v -> setGlobal k $! f v

runGlobalVarsAction :: (Hashable k, Eq k) =>
                       Sem r k -> HashMap k v -> Sem (GlobalVars k v ': r) a -> Sem r (HashMap k v, a)
runGlobalVarsAction gensym init_ = runState init_ . reinterpret
                                   (\case
                                     DefineGlobal v -> do
                                       k <- raise gensym
                                       modify' (HashMap.insert k v)
                                       return k
                                     GetGlobal k -> gets (fromJust . HashMap.lookup k)
                                     SetGlobal k v -> modify' (HashMap.insert k v))

runGlobalVarsUnique :: HashMap Integer v -> Sem (GlobalVars Integer v ': r) a -> Sem r (HashMap Integer v, a)
runGlobalVarsUnique init_ = runUniqueInt . runGlobalVarsAction unique init_ . raiseUnder

evalGlobalVarsUnique :: Sem (GlobalVars Integer v ': r) a -> Sem r a
evalGlobalVarsUnique = fmap snd . runGlobalVarsUnique HashMap.empty
