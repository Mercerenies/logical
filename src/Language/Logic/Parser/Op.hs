{-# LANGUAGE RecordWildCards #-}

module Language.Logic.Parser.Op where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List.NonEmpty(NonEmpty(..))
--import qualified Data.List.NonEmpty as NonEmpty

data OpTerm a = Term a | OpTerm String

data Assoc = AssocLeft | AssocRight | AssocNone
             deriving (Show, Read, Eq, Ord, Enum)

data Op = Op Int Assoc

data TermComp a = TermComp {
      termInfix :: a -> String -> a -> a,
      termPrefix :: String -> a -> a
    }

data OpError = OpError String

instance Show OpError where
    showsPrec _ (OpError s) = (s ++)

instance Functor OpTerm where
    fmap f (Term a) = Term (f a)
    fmap _ (OpTerm s) = OpTerm s

newtype OpTable = OpTable (Map String Op)

defaultOp :: Op
defaultOp = Op 0 AssocNone -- TODO Actually decide this

getPrec :: String -> OpTable -> Op
getPrec s (OpTable m) = Map.findWithDefault defaultOp s m

resolvePrec :: forall a. Show a => OpTable -> TermComp a -> NonEmpty (OpTerm a) -> Either OpError a
resolvePrec table (TermComp {..}) (x0 :| xs0) = go [] [] (x0:xs0)
    where go :: [a] -> [String] -> [OpTerm a] -> Either OpError a
          go [out] [] [] = Right out
          go out [] [] = Left (OpError $ "Tokens left after operator parsing: " ++ show out)
          go out (op:ops) [] = popOp out op ops []
          go out ops (Term a : xs) = go (a : out) ops xs
          go out [] (OpTerm op : xs) = go out [op] xs
          go out (op' : ops) (OpTerm op : xs) =
              let Op pr  as  = getPrec op  table
                  Op pr' _   = getPrec op' table in
              case pr' `compare` pr of
                GT -> popOp out op' ops (OpTerm op : xs)
                EQ | as == AssocLeft -> popOp out op' ops (OpTerm op : xs)
                _ -> go out (op : op' : ops) xs
          popOp :: [a] -> String -> [String] -> [OpTerm a] -> Either OpError a
          popOp (arg2:arg1:out) op ops xs = go (termInfix arg1 op arg2 : out) ops xs
          popOp _ op _ _ = Left (OpError $ "Not enough arguments to operator " ++ op)
