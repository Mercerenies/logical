
module Language.Logic.Util(sepBy, oneOf, classify) where

import Control.Applicative
import Data.List(foldl')
import Data.Map(Map)
import qualified Data.Map as Map

sepBy :: Foldable t => ShowS -> t ShowS -> ShowS
sepBy delim = maybe id id . foldr go Nothing
    where go x Nothing = Just x
          go x (Just y) = Just (x . delim . y)

oneOf :: (Functor t, Foldable t, Alternative f) => t a -> f a
oneOf = foldr (<|>) empty . fmap pure

classify :: Ord k => (a -> k) -> [a] -> Map k [a]
classify f = fmap reverse . foldl' go Map.empty
    where go m x = Map.alter (alt x) (f x) m
          alt x Nothing = Just [x]
          alt x (Just xs) = Just (x:xs)
