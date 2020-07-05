
module Language.Logic.Term.Parsed(PTerm(..), PFact(..),
                                  ptermToTerm, pfactToFact, quoteAll) where

-- Like Language.Logic.Term but remembers whether or not the term was quoted.

import Language.Logic.Term
import Language.Logic.Number(Number(..))
import Language.Logic.Term.Handle

import qualified Data.Text as T

data PTerm = PTermVar String
           | PTermNum Number
           | PTermString T.Text
           | PTermCompound String Bool [PTerm]
           | PTermHandle Handle
             deriving (Eq, Ord)

data PFact = PFact String Bool [PTerm]
             deriving (Eq, Ord)

ptermToTerm :: PTerm -> Term
ptermToTerm (PTermVar s) = TermVar s
ptermToTerm (PTermNum n) = TermNum n
ptermToTerm (PTermString s) = TermString s
ptermToTerm (PTermCompound s _ ts) = TermCompound s $ fmap ptermToTerm ts
ptermToTerm (PTermHandle h) = TermHandle h

pfactToFact :: PFact -> Fact
pfactToFact (PFact s _ ts) = Fact s $ fmap ptermToTerm ts

quoteAll :: PTerm -> PTerm
quoteAll (PTermCompound s _ ts) = PTermCompound s True $ fmap quoteAll ts
quoteAll t = t

-- TODO The Show instances below don't print quotes for quoted terms

instance Show PTerm where
    showsPrec n = showsPrec n . ptermToTerm

instance Show PFact where
    showsPrec n = showsPrec n . pfactToFact
