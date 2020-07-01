
module Language.Logic.Util(sepBy, oneOf, classify, traverseKeys, hoistMaybe) where

import Control.Applicative
import Data.List(foldl')
import Data.Map(Map)
import qualified Data.Map as Map

sepBy :: Foldable t => ShowS -> t ShowS -> ShowS
sepBy delim = maybe id id . foldr go Nothing
    where go x Nothing = Just x
          go x (Just y) = Just (x . delim . y)

oneOf :: (Functor t, Foldable t, Alternative f) => t (f a) -> f a
oneOf = foldr (<|>) empty -- TODO This is just asum....

classify :: Ord k => (a -> k) -> [a] -> Map k [a]
classify f = fmap reverse . foldl' go Map.empty
    where go m x = Map.alter (alt x) (f x) m
          alt x Nothing = Just [x]
          alt x (Just xs) = Just (x:xs)

traverseKeys :: (Applicative t, Ord k2) => (k1 -> t k2) -> Map k1 a -> t (Map k2 a)
traverseKeys f = fmap Map.fromList . traverse go . Map.toList
    where go (k, v) = liftA2 (,) (f k) (pure v)

hoistMaybe :: Alternative f => Maybe a -> f a
hoistMaybe Nothing = empty
hoistMaybe (Just x) = pure x
