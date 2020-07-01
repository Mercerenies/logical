{-# LANGUAGE DerivingVia, DeriveFunctor #-}

module Language.Logic.Optimization where

import Control.Category hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Kind

newtype Optimization (m :: Type -> Type) a b = Optimization { runOptimization :: a -> m b }
    deriving (Arrow, ArrowChoice, ArrowApply, ArrowLoop,
              ArrowPlus, ArrowZero, Category)
    via Kleisli m
    deriving stock (Functor)

-- Can't derive Applicative or Monad via because I'm evidently using a
-- version of base from before they were in Kleisli, so I'll write
-- them myself.

instance Applicative m => Applicative (Optimization m a) where
    pure x = Optimization (\_ -> pure x)
    Optimization ff <*> Optimization xx = Optimization (\a -> ff a <*> xx a)

instance Monad m => Monad (Optimization m a) where
    return = pure
    Optimization xx >>= f = Optimization (\a -> xx a >>= flip runOptimization a . f)

instance Alternative m => Alternative (Optimization m a) where
    empty = Optimization (const empty)
    Optimization xx <|> Optimization yy = Optimization (\a -> xx a <|> yy a)

instance MonadPlus m => MonadPlus (Optimization m a)
