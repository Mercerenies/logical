
module Language.Logic.Term(Term(..), Fact(..),
                           factHead, factBody,
                           freeVars, freeVarsInFact, safeVar,
                           renameVars, renameInFact) where

import qualified Language.Logic.Util as Util

data Term = TermVar String
          | TermInt Integer
          | TermCompound String [Term]
            deriving (Eq, Ord)

instance Show Term where
    showsPrec _ (TermVar v) = (v ++)
    showsPrec _ (TermInt n) = shows n
    showsPrec _ (TermCompound s args) = (s ++) . ("(" ++) . args' . (")" ++)
        where args' = Util.sepBy ("," ++) $ fmap shows args

data Fact = Fact String [Term]
            deriving (Eq, Ord)

instance Show Fact where
    showsPrec _ (Fact s args) = (s ++) . ("(" ++) . args' . (")" ++)
        where args' = Util.sepBy ("," ++) $ fmap shows args

factHead :: Fact -> String
factHead (Fact s _) = s

factBody :: Fact -> [Term]
factBody (Fact _ t) = t

freeVars :: Term -> [String]
freeVars (TermVar s) = [s]
freeVars (TermInt {}) = []
freeVars (TermCompound _ ts) = concatMap freeVars ts

freeVarsInFact :: Fact -> [String]
freeVarsInFact (Fact _ xs) = concatMap freeVars xs

safeVar :: [String] -> String -> String
safeVar xs s = head $ filter (`notElem` xs) possibilities
    where possibilities = s : [s ++ show n | n <- [0 :: Int ..]]

renameVars :: [String] -> Term -> Term
renameVars xs t = go t
    where go (TermVar s) = TermVar $ replaceVar s
          go (TermInt n) = TermInt n
          go (TermCompound s args) = TermCompound s $ fmap go args
          xs' = xs ++ freeVars t
          replaceVar s
              | s `elem` xs = safeVar xs' s
              | otherwise = s

renameInFact :: [String] -> Fact -> Fact
renameInFact xs (Fact h ts) =
    case renameVars xs (TermCompound h ts) of
      TermCompound _ ts' -> Fact h ts'
      _ -> error "renameVars changed shape in renameInFact"
