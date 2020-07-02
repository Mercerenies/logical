
module Language.Logic.Compile where

import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Code
import Language.Logic.SymbolTable.Monad
import Language.Logic.Tagged
import qualified Language.Logic.Util as Util

import Polysemy
import qualified Data.Text as T

internTag :: Member (SymbolTableState SymbolId) r => String -> Sem r (Tagged Atom SymbolId)
internTag s = Tagged (Atom s) <$> intern (T.pack s)

compileClause :: Member (SymbolTableState SymbolId) r =>
                 Clause String Fact -> Sem r (Clause (Tagged Atom SymbolId) CFact)
compileClause (StdClause f fs) = StdClause <$> compileFact f <*> mapM compileFact fs
compileClause (PrimClause k g) = PrimClause <$> internTag k <*> pure (g . cfactToFact)

compileClause' :: Member (SymbolTableState SymbolId) r =>
                  Clause String CFact -> Sem r (Clause (Tagged Atom SymbolId) CFact)
compileClause' (StdClause f fs) = pure (StdClause f fs)
compileClause' (PrimClause k g) = PrimClause <$> internTag k <*> pure g

compileBody :: Member (SymbolTableState SymbolId) r =>
               CodeBody String Fact -> Sem r (CodeBody (Tagged Atom SymbolId) CFact)
compileBody (CodeBody m) = CodeBody <$> m'
    where m' = Util.traverseKeys internTag m >>= traverse (traverse compileClause)

compound :: Member (SymbolTableState SymbolId) r => String -> [CTerm] -> Sem r CTerm
compound hd tl = CTermCompound <$> internTag hd <*> pure tl

atom :: Member (SymbolTableState SymbolId) r => String -> Sem r CTerm
atom hd = compound hd []

compound' :: Member (SymbolTableState SymbolId) r => String -> [Sem r CTerm] -> Sem r CTerm
compound' hd tl = sequence tl >>= compound hd
