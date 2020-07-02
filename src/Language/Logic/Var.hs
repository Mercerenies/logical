
module Language.Logic.Var where

import Language.Logic.Unique
import Language.Logic.Term.Compiled
import Language.Logic.Code
import qualified Language.Logic.Names as Names

import Polysemy

freshVar :: Member (Unique Int) r => String -> Sem r String
freshVar s = uniques (\n -> s ++ show n)

safeFreshVar :: Member (Unique Int) r => [String] -> String -> Sem r String
safeFreshVar xs s = do
  v <- freshVar s
  if v `elem` xs then
      safeFreshVar xs s -- Try again
  else
      return v

replaceUnderscores :: Member (Unique Int) r => Clause k CFact -> Sem r (Clause k CFact)
replaceUnderscores (pc@(PrimClause _ _)) = pure pc
replaceUnderscores (StdClause hd tl) =
      StdClause <$> traverseVarsInCFact go hd <*> mapM (traverseVarsInCFact go) tl
    where allVars = freeVarsInCFact hd ++ concatMap freeVarsInCFact tl
          prefix = "_U"
          go x | x == Names.blankVar = CTermVar <$> safeFreshVar allVars prefix
               | otherwise = pure $ CTermVar x
