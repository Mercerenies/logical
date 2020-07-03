
module Language.Logic.Term(Term(..), Fact(..), Atom(..),
                           freeVars, freeVarsInFact, safeVar,
                           renameVars, renameInFact,
                           traverseVars, traverseVarsInFact) where

import qualified Language.Logic.Util as Util
import qualified Language.Logic.Names as Names
import Language.Logic.Number(Number(..))
import Language.Logic.Term.Handle

import Data.Char

data Term = TermVar String
          | TermNum Number
          | TermCompound String [Term]
          | TermHandle Handle
            deriving (Eq, Ord)

data Fact = Fact String [Term]
            deriving (Eq, Ord)

newtype Atom = Atom String
    deriving (Eq, Ord)

instance Show Term where
    showsPrec _ (TermVar v) = (v ++)
    showsPrec _ (TermNum n) = shows n
    showsPrec _ (xs@ TermCompound {}) | Just xs' <- toProperList xs = shows xs'
    showsPrec _ (TermCompound s args) = showsAtomic s . ("(" ++) . args' . (")" ++)
        where args' = Util.sepBy ("," ++) $ fmap shows args
    showsPrec _ (TermHandle handle) = ("<handle " ++) . shows handle . (">" ++)

instance Show Fact where
    showsPrec n (Fact s args) = showsPrec n (TermCompound s args)

instance Show Atom where
    showsPrec _ (Atom s) = showsAtomic s

-- TODO If we end up supporting cyclic lists, we'll need to modify
-- this to detect them.
toProperList :: Term -> Maybe [Term]
toProperList (TermCompound x []) | x == Names.emptyList = Just []
toProperList (TermCompound x [a, b]) | x == Names.consList = (a :) <$> toProperList b
toProperList _ = Nothing

isStdAtom :: String -> Bool
isStdAtom [] = False
isStdAtom xs | not $ all (\x -> isAlpha x || isDigit x || x == '_') xs = False
isStdAtom (x:_) | isDigit x = False
isStdAtom (x:_) | not (isLower x) = False
isStdAtom _ = True

showsAtomic :: String -> ShowS
showsAtomic s =
    if isStdAtom s then
        (s ++)
    else
        ("`" ++) . (s ++) . ("`" ++)

freeVars :: Term -> [String]
freeVars (TermVar s) = [s]
freeVars (TermNum {}) = []
freeVars (TermCompound _ ts) = concatMap freeVars ts
freeVars (TermHandle _) = []

freeVarsInFact :: Fact -> [String]
freeVarsInFact (Fact _ xs) = concatMap freeVars xs

safeVar :: [String] -> String -> String
safeVar xs s = head $ filter (`notElem` xs) possibilities
    where possibilities = s : [s ++ show n | n <- [0 :: Int ..]]

renameVars :: [String] -> Term -> Term
renameVars xs t = go t
    where go (TermVar s) = TermVar $ replaceVar s
          go (TermNum n) = TermNum n
          go (TermCompound s args) = TermCompound s $ fmap go args
          go (TermHandle h) = TermHandle h
          xs' = xs ++ freeVars t
          replaceVar s
              | s `elem` xs = safeVar xs' s
              | otherwise = s

renameInFact :: [String] -> Fact -> Fact
renameInFact xs (Fact h ts) =
    case renameVars xs (TermCompound h ts) of
      TermCompound _ ts' -> Fact h ts'
      _ -> error "renameVars changed shape in renameInFact"

traverseVars :: Applicative f => (String -> f Term) -> Term -> f Term
traverseVars f = go
    where go (TermVar s) = f s
          go (TermNum n) = pure (TermNum n)
          go (TermCompound s args) = TermCompound s <$> traverse go args
          go (TermHandle h) = pure (TermHandle h)

traverseVarsInFact :: Applicative f => (String -> f Term) -> Fact -> f Fact
traverseVarsInFact f (Fact h ts) = Fact h <$> traverse (traverseVars f) ts
