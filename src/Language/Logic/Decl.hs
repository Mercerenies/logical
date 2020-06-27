
module Language.Logic.Decl where

import Language.Logic.Code
import Language.Logic.Term(Fact)
import qualified Language.Logic.Parser.Op as Op

data Decl = OperatorDecl Op.OpA Op.Op

data ClauseOrDecl = Clause (Clause String Fact) | Decl Decl

instance Show Decl where
    showsPrec _ (OperatorDecl (Op.OpA fx name) (Op.Op pr as)) =
        ("operator " ++) . lhs . (" : " ++) . rhs
            where lhs = fx' . (" " ++) . (name ++)
                  rhs = as' . (" " ++) . shows pr
                  fx' = case fx of
                          Op.Infix -> ("infix" ++)
                          Op.Prefix -> ("prefix" ++)
                  as' = case as of
                          Op.AssocLeft -> ("left" ++)
                          Op.AssocRight -> ("right" ++)

instance Show ClauseOrDecl where
    showsPrec n (Clause c) = showsPrec n c
    showsPrec n (Decl d) = showsPrec n d

toClause :: ClauseOrDecl -> Maybe (Clause String Fact)
toClause (Clause c) = Just c
toClause (Decl _) = Nothing

toDecl :: ClauseOrDecl -> Maybe Decl
toDecl (Clause _) = Nothing
toDecl (Decl d) = Just d
