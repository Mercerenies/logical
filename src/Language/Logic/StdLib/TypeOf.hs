
module Language.Logic.StdLib.TypeOf where

import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Term.Handle
import Language.Logic.Compile
import Language.Logic.Tagged
import Language.Logic.SymbolTable.Monad
import Language.Logic.Number

import Polysemy

import Data.Ratio(numerator, denominator)

data TermType = TyVar | TyNum TermNumType | TyString |
                TyCompound (Tagged Atom SymbolId) Int | TyHandle TermHandleType
                deriving (Show, Eq)

data TermNumType = TyRatio Integer Integer | TyFloat
                   deriving (Show, Read, Eq, Ord)

data TermHandleType = TyRef
                      deriving (Show, Read, Eq, Ord, Enum)

typeOf :: CTerm -> TermType
typeOf (CTermVar _) = TyVar
typeOf (CTermNum (NumRat r)) = TyNum (TyRatio (numerator r) (denominator r))
typeOf (CTermNum (NumFloat _)) = TyNum TyFloat
typeOf (CTermString _) = TyString
typeOf (CTermCompound hd tl) = TyCompound hd (length tl)
typeOf (CTermHandle (HandleRef _)) = TyHandle TyRef

typeToTerm :: Member (SymbolTableState SymbolId) r => TermType -> Sem r CTerm
typeToTerm TyVar = atom "variable"
typeToTerm (TyNum t) = compound' "number" [case t of
                                             TyRatio n d -> compound "ratio" [CTermNum (fromIntegral n),
                                                                              CTermNum (fromIntegral d)]
                                             TyFloat -> atom "float"]
typeToTerm TyString = atom "string"
typeToTerm (TyCompound hd n) = compound "compound" [CTermCompound hd [], CTermNum (fromIntegral n)]
typeToTerm (TyHandle t) = compound' "handle" [case t of
                                                TyRef -> atom "ref"]

typeOf' :: Member (SymbolTableState SymbolId) r => CTerm -> Sem r CTerm
typeOf' = typeToTerm . typeOf
