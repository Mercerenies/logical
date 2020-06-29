
module Language.Logic.Choice(Choice(..), empty, choose, once,
                             nonDetToChoice, runChoice0, runChoice, runChoiceMaybe, runChoiceOnce,
                             runNonDetOnce) where

import Polysemy
import qualified Polysemy.Error as Err
import qualified Polysemy.NonDet as ND

import Control.Applicative(Alternative((<|>)))
import qualified Control.Applicative as App
import Control.Arrow((>>>))

data Choice m a where
    Empty :: Choice m a
    Choose :: m a -> m a -> Choice m a
    Once :: m a -> Choice m a

-- Error type used to abort the computation after one success.
data PrivateShortCircuit a = PrivateShortCircuit { unPrivateShortCircuit :: a }

makeSem ''Choice

nonDetToChoice :: Member Choice r => Sem (ND.NonDet ': r) a -> Sem r a
nonDetToChoice = interpretH $ \case
                 ND.Empty -> empty
                 ND.Choose a b -> do
                   a' <- runT a
                   b' <- runT b
                   let a'' = raise $ nonDetToChoice a'
                       b'' = raise $ nonDetToChoice b'
                   choose a'' b''

runChoice0 :: forall f r a. Alternative f =>
              (forall r1. Sem (ND.NonDet ': r1) a -> Sem r1 (f a)) -> Sem (Choice ': r) a -> Sem r (f a)
runChoice0 f = f . reinterpretH go
    where go :: forall m x f1. Choice m x -> Sem (WithTactics Choice f1 m (ND.NonDet : r)) (f1 x)
          go Empty = App.empty
          go (Choose a b) = do
            a' <- runT a
            let a'' = raise $ interpretH go a'
            a'' <|> do
              b' <- runT b
              raise $ interpretH go b'
          go (Once a) = do
            a' <- runT a
            a'' <- raise $ runChoiceOnce a'
            case a'' of
              Nothing -> App.empty
              Just x -> pure x

runChoice :: Alternative f => Sem (Choice ': r) a -> Sem r (f a)
runChoice = runChoice0 ND.runNonDet

runChoiceMaybe :: Sem (Choice ': r) a -> Sem r (Maybe a)
runChoiceMaybe = runChoice0 ND.runNonDetMaybe

runChoiceOnce :: Sem (Choice ': r) a -> Sem r (Maybe a)
runChoiceOnce = runChoice0 runNonDetOnce

-- TODO If this is a performance problem (which it may very well end
-- up being), provide a version that runs in IO.
runNonDetOnce :: forall r a. Sem (ND.NonDet ': r) a -> Sem r (Maybe a)
runNonDetOnce =
    raiseUnder >>>
    (>>= Err.throw . PrivateShortCircuit) >>>
    ND.runNonDet @Maybe >>>
    Err.runError @(PrivateShortCircuit a) >>>
    fmap (either (Just . unPrivateShortCircuit) id)

-- TODO If we're feeling particularly ambitious, we could consider
-- trying to run the whole thing in one interpreter (rather than
-- giving each Once its own interpreter) and make PrivateShortCircuit
-- more sophisticated so it can catch the right exceptions and ignore
-- others.
