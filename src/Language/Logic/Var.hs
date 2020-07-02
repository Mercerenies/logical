
module Language.Logic.Var where

import Language.Logic.Unique

import Polysemy

freshVar :: Member (Unique Int) r => String -> Sem r String
freshVar s = uniques (\n -> s ++ show n)


