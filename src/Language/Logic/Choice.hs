
module Language.Logic.Choice where

import Polysemy
import qualified Polysemy.NonDet as ND

import Control.Applicative(Alternative((<|>)))
import qualified Control.Applicative as App

data Choice m a where
    Empty :: Choice m a
    Choose :: m a -> m a -> Choice m a

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

runChoice :: forall f r a. Alternative f => Sem (Choice ': r) a -> Sem r (f a)
runChoice = ND.runNonDet . reinterpretH go
    where go :: forall m x f1. Choice m x -> Sem (WithTactics Choice f1 m (ND.NonDet : r)) (f1 x)
          go Empty = App.empty
          go (Choose a b) = do
            a' <- runT a
            b' <- runT b
            let a'' = raise $ interpretH go a'
                b'' = raise $ interpretH go b'
            a'' <|> b''
