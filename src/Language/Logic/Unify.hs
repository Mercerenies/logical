
module Language.Logic.Unify(Assumptions(..), AssumptionState(..), tellAssumption, getAssumptions,
                            UnifyError(..), runAssumptionState, evalAssumptionState,
                            assumption, assume,
                            subOnce, subOnceInFact,
                            doSub, doSubFact, subAndUnify, fullUnify) where

import Language.Logic.Term
--import Language.Logic.Term.Compiled

import Polysemy
import Polysemy.State
import Polysemy.Error

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad

newtype Assumptions = Assumptions (Map String Term)
    deriving (Eq, Show) -- TODO Make the show instance pretty

data AssumptionState m a where
    TellAssumption :: Assumptions -> AssumptionState m ()
    GetAssumptions :: AssumptionState m Assumptions

makeSem ''AssumptionState

data UnifyError = NoUnify Term Term
                | OccursCheck String Term
                  deriving (Show) -- TODO Make the show instance pretty

instance Semigroup Assumptions where
    -- TODO I'm pretty sure this is associative. Can we prove it?
    Assumptions a <> Assumptions b =
        let a' = fmap (subOnce $ Assumptions b) a in
        Assumptions $ Map.unionWith (\_ y -> y) a' b

instance Monoid Assumptions where
    mempty = Assumptions Map.empty

runAssumptionState :: Member (Error UnifyError) r => Sem (AssumptionState ': r) a -> Sem r (Assumptions, a)
runAssumptionState =
    runLazyState mempty .
    reinterpret (\case
      TellAssumption asm -> modify' (<> asm) >> get >>= occursCheck
      GetAssumptions -> get)

evalAssumptionState :: Member (Error UnifyError) r => Sem (AssumptionState ': r) a -> Sem r a
evalAssumptionState = fmap snd . runAssumptionState

assumption :: String -> Term -> Assumptions
assumption s t = Assumptions (Map.singleton s t)

assume :: Member AssumptionState r => String -> Term -> Sem r ()
assume s t = tellAssumption (assumption s t)

occursCheck :: forall r. Member (Error UnifyError) r => Assumptions -> Sem r ()
occursCheck (Assumptions m) = void $ Map.traverseWithKey go m
    where go :: String -> Term -> Sem r ()
          go _ (TermVar _) = pure () -- Trivial case, not a problem even if technically cyclic
          go k term
              | k `elem` freeVars term = throw (OccursCheck k term)
              | otherwise = pure ()

subOnce :: Assumptions -> Term -> Term
subOnce (Assumptions m) t = go t
    where go (TermVar v)
              | Just t' <- Map.lookup v m = t'
              | otherwise = TermVar v
          go (TermCompound s ts) = TermCompound s (fmap go ts)
          go (TermNum n) = TermNum n

subOnceInFact :: Assumptions -> Fact -> Fact
subOnceInFact asm (Fact h ts) = Fact h $ fmap (subOnce asm) ts

doSub :: Member AssumptionState r => Term -> Sem r Term
doSub t = (\asm -> subOnce asm t) <$> getAssumptions

doSubFact :: Member AssumptionState r => Fact -> Sem r Fact
doSubFact (Fact h ts) = Fact h <$> mapM doSub ts

unify0 :: (Member AssumptionState r, Member (Error UnifyError) r) => Term -> Term -> Sem r Term
unify0 a b | a == b = pure a
unify0 (TermVar a) (TermVar b)
    | a < b = assume a (TermVar b) >> pure (TermVar b)
    | otherwise = assume b (TermVar a) >> pure (TermVar a)
unify0 (TermVar a) b = assume a b >> pure b
unify0 a (TermVar b) = assume b a >> pure a
unify0 (TermCompound s t) (TermCompound u v)
    | s == u && length t == length v = TermCompound s <$> zipWithM subAndUnify t v
unify0 a b = throw (NoUnify a b)

subAndUnify :: (Member AssumptionState r, Member (Error UnifyError) r) => Term -> Term -> Sem r Term
subAndUnify a b = do
  a' <- doSub a
  b' <- doSub b
  unify0 a' b'

fullUnify :: Member (Error UnifyError) r => Term -> Term -> Sem r (Assumptions, Term)
fullUnify a b = runAssumptionState (subAndUnify a b)
