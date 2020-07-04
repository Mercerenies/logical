
module Language.Logic.Unify.Compiled(Assumptions(..), AssumptionState(..), tellAssumption, getAssumptions,
                                     runAssumptionState, evalAssumptionState,
                                     assumption, assume,
                                     subOnce, subOnceInFact,
                                     doSub, doSubFact, subAndUnify, fullUnify) where

import qualified Language.Logic.Unify as U
import Language.Logic.Term.Compiled

import Polysemy
import Polysemy.State
import Polysemy.Error

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad

newtype Assumptions = Assumptions (Map String CTerm)
    deriving (Eq, Show) -- TODO Make the show instance pretty

data AssumptionState m a where
    TellAssumption :: Assumptions -> AssumptionState m ()
    GetAssumptions :: AssumptionState m Assumptions

makeSem ''AssumptionState

instance Semigroup Assumptions where
    -- TODO I'm pretty sure this is associative. Can we prove it?
    Assumptions a <> Assumptions b =
        let a' = fmap (subOnce $ Assumptions b) a in
        Assumptions $ Map.unionWith (\_ y -> y) a' b

instance Monoid Assumptions where
    mempty = Assumptions Map.empty

noUnify :: CTerm -> CTerm -> U.UnifyError
noUnify a b = U.NoUnify (ctermToTerm a) (ctermToTerm b)

runAssumptionState :: Member (Error U.UnifyError) r => Sem (AssumptionState ': r) a -> Sem r (Assumptions, a)
runAssumptionState =
    runLazyState mempty .
    reinterpret (\case
      TellAssumption asm -> modify' (<> asm) >> get >>= occursCheck
      GetAssumptions -> get)

evalAssumptionState :: Member (Error U.UnifyError) r => Sem (AssumptionState ': r) a -> Sem r a
evalAssumptionState = fmap snd . runAssumptionState

assumption :: String -> CTerm -> Assumptions
assumption s t = Assumptions (Map.singleton s t)

assume :: Member AssumptionState r => String -> CTerm -> Sem r ()
assume s t = tellAssumption (assumption s t)

occursCheck :: forall r. Member (Error U.UnifyError) r => Assumptions -> Sem r ()
occursCheck (Assumptions m) = void $ Map.traverseWithKey go m
    where go :: String -> CTerm -> Sem r ()
          go _ (CTermVar _) = pure () -- Trivial case, not a problem even if technically cyclic
          go k term
              | k `elem` freeVarsC term = throw (U.OccursCheck k (ctermToTerm term))
              | otherwise = pure ()

subOnce :: Assumptions -> CTerm -> CTerm
subOnce (Assumptions m) t = go t
    where --go CTermBlank = CTermBlank
          go (CTermVar v)
              | Just t' <- Map.lookup v m = t'
              | otherwise = CTermVar v
          go (CTermCompound s ts) = CTermCompound s (fmap go ts)
          go (CTermNum n) = CTermNum n
          go (CTermString txt) = CTermString txt
          go (CTermHandle h) = CTermHandle h

subOnceInFact :: Assumptions -> CFact -> CFact
subOnceInFact asm (CFact h ts) = CFact h $ fmap (subOnce asm) ts

doSub :: Member AssumptionState r => CTerm -> Sem r CTerm
doSub t = (\asm -> subOnce asm t) <$> getAssumptions

doSubFact :: Member AssumptionState r => CFact -> Sem r CFact
doSubFact (CFact h ts) = CFact h <$> mapM doSub ts

unify0 :: (Member AssumptionState r, Member (Error U.UnifyError) r) => CTerm -> CTerm -> Sem r CTerm
unify0 a b | a == b = pure a
--unify0 CTermBlank b = pure b
--unify0 a CTermBlank = pure a
unify0 (CTermVar a) (CTermVar b)
    | a < b = assume a (CTermVar b) >> pure (CTermVar b)
    | otherwise = assume b (CTermVar a) >> pure (CTermVar a)
unify0 (CTermVar a) b = assume a b >> pure b
unify0 a (CTermVar b) = assume b a >> pure a
unify0 (CTermCompound s t) (CTermCompound u v)
    | s == u && length t == length v = CTermCompound s <$> zipWithM subAndUnify t v
unify0 a b = throw (noUnify a b)

subAndUnify :: (Member AssumptionState r, Member (Error U.UnifyError) r) => CTerm -> CTerm -> Sem r CTerm
subAndUnify a b = do
  a' <- doSub a
  b' <- doSub b
  unify0 a' b'

fullUnify :: Member (Error U.UnifyError) r => CTerm -> CTerm -> Sem r (Assumptions, CTerm)
fullUnify a b = runAssumptionState (subAndUnify a b)
