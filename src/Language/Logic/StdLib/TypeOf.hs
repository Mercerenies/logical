
module Language.Logic.StdLib.TypeOf where

import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Term.Handle
import Language.Logic.Compile
import Language.Logic.Tagged
import Language.Logic.SymbolTable.Monad
import Language.Logic.Number

import Polysemy

data TermType = TyVar | TyNum TermNumType | TyCompound (Tagged Atom SymbolId) Int | TyHandle TermHandleType
                deriving (Show, Eq)

data TermNumType = TyRatio | TyFloat
                   deriving (Show, Read, Eq, Ord, Enum)

data TermHandleType = TyRef
                      deriving (Show, Read, Eq, Ord, Enum)

typeOf :: CTerm -> TermType
typeOf (CTermVar _) = TyVar
typeOf (CTermNum (NumRat _)) = TyNum TyRatio
typeOf (CTermNum (NumFloat _)) = TyNum TyFloat
typeOf (CTermCompound hd tl) = TyCompound hd (length tl)
typeOf (CTermHandle (HandleRef _)) = TyHandle TyRef

typeToTerm :: Member (SymbolTableState SymbolId) r => TermType -> Sem r CTerm
typeToTerm TyVar = atom "variable"
typeToTerm (TyNum t) = compound' "number" [case t of
                                             TyRatio -> atom "ratio"
                                             TyFloat -> atom "float"]
typeToTerm (TyCompound hd n) = compound "compound" [CTermCompound hd [], CTermNum (fromIntegral n)]
typeToTerm (TyHandle t) = compound' "handle" [case t of
                                                TyRef -> atom "ref"]

typeOf' :: Member (SymbolTableState SymbolId) r => CTerm -> Sem r CTerm
typeOf' = typeToTerm . typeOf
