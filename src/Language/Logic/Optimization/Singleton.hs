
module Language.Logic.Optimization.Singleton where

import Language.Logic.Optimization
import Language.Logic.Code
import Language.Logic.Term.Compiled

import Control.Monad.Trans.Writer.CPS

import Data.Map(Map)
import qualified Data.Map as Map

removeSingletons :: Ord k => Optimization m (Clause k CTerm) (Clause k CTerm)
removeSingletons = Optimization undefined -- /////

trackSingletons :: CFact -> SingletonTracker
trackSingletons = snd . runWriter . traverseVarsInCFact go
    where go s = CTermVar s <$ tell (oneVar s)

oneVar :: String -> SingletonTracker
oneVar s = SingletonTracker (Map.singleton s IsSingleton)

newtype SingletonTracker = SingletonTracker (Map String IsSingleton)

data IsSingleton = IsSingleton | UsedManyTimes
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Semigroup IsSingleton where
    _ <> _ = UsedManyTimes

instance Semigroup SingletonTracker where
    SingletonTracker m <> SingletonTracker m' = SingletonTracker (Map.unionWith (<>) m m')

instance Monoid SingletonTracker where
    mempty = SingletonTracker mempty
