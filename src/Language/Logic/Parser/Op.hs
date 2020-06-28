{-# LANGUAGE RecordWildCards #-}

module Language.Logic.Parser.Op(OpTerm(..), Fixity(..), Assoc(..), OpA(..), Op(..),
                                TermComp(..), OpError(..), OpTable(..),
                                defaultOp, getPrec, resolvePrec) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List.NonEmpty(NonEmpty(..))
--import qualified Data.List.NonEmpty as NonEmpty

data OpTerm a = Term a | OpTerm String

data Fixity = Infix | Prefix
              deriving (Show, Read, Eq, Ord, Enum)

data OpA = OpA Fixity String
           deriving (Eq, Ord)

data Assoc = AssocLeft | AssocRight
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

newtype OpTable = OpTable (Map OpA Op)

defaultOp :: Op
defaultOp = Op 50 AssocLeft

getPrec :: OpA -> OpTable -> Op
getPrec s (OpTable m) =
    let OpA fx _ = s
        Op pr as = Map.findWithDefault defaultOp s m in
    if fx == Prefix then Op pr AssocRight else Op pr as -- Prefix operators always right-associate

resolvePrec :: forall a. Show a => OpTable -> TermComp a -> NonEmpty (OpTerm a) -> Either OpError a
resolvePrec table (TermComp {..}) (x0 :| xs0) = verifySeq (x0 :| xs0) >> go Prefix [] [] (x0:xs0)
    where -- The Fixity "state" argument tells us which fixity to
          -- expect if we see an operator. At the beginning of
          -- parsing, we can only see prefix operators. After a term,
          -- we will always see exactly one infix operator, followed
          -- by zero or more prefix operators.
          go :: Fixity -> [a] -> [OpA] -> [OpTerm a] -> Either OpError a
          go _ [out] [] [] = Right out
          go _ out [] [] = Left (OpError $ "Tokens left after operator parsing: " ++ show out)
          go fx out (op:ops) [] = popOp fx out op ops []
          go _ out ops (Term a : xs) = go Infix (a : out) ops xs
          go fx out [] (OpTerm op : xs) = go Prefix out [OpA fx op] xs
          go fx out (op' : ops) (OpTerm nxt : xs) =
              let op = OpA fx nxt
                  Op pr  as  = getPrec op  table
                  Op pr' _   = getPrec op' table in
              case pr' `compare` pr of
                _ | fx == Prefix -> go Prefix out (op : op' : ops) xs -- Always push prefixes
                GT -> popOp fx out op' ops (OpTerm nxt : xs)
                EQ | as == AssocLeft -> popOp fx out op' ops (OpTerm nxt : xs)
                _ -> go Prefix out (op : op' : ops) xs
          popOp :: Fixity -> [a] -> OpA -> [OpA] -> [OpTerm a] -> Either OpError a
          popOp fx (arg2:arg1:out) (OpA Infix op) ops xs = go fx (termInfix arg1 op arg2 : out) ops xs
          popOp fx (arg1:out) (OpA Prefix op) ops xs = go fx (termPrefix op arg1 : out) ops xs
          popOp _ _ (OpA p op) _ _ = Left (OpError $ "Not enough arguments to operator " ++ op ++ " (" ++ show p ++ ")")

-- This is run at the beginning of resolvePrec and verifies certain
-- invariants about the sequence of operators provided.
verifySeq :: Show a => NonEmpty (OpTerm a) -> Either OpError ()
verifySeq (Term _ :| []) = pure () -- Consumed the whole list, so we're done
verifySeq (Term a :| Term b : _) = Left (OpError $ "Adjacent terms in operator parse: " ++ show a ++ ", " ++ show b)
verifySeq (Term _ :| OpTerm b : []) = Left (OpError $ "Trailing infix operator " ++ show b ++ " in expression")
verifySeq (Term _ :| OpTerm _ : x : xs) = verifySeq (x :| xs) -- Consume term and infix op
verifySeq (OpTerm a :| []) = Left (OpError $ "Trailing prefix operator " ++ show a ++ " in expression")
verifySeq (OpTerm _ :| x : xs) = verifySeq (x :| xs) -- Consume prefix op
