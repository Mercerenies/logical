
module Language.Logic.Error where

-- These errors are actual runtime errors, not simple failures which
-- backtrack. Generally, the presence of one of these errors will
-- abort the whole program. Later, we may add some way to "catch"
-- errors.

data RuntimeError = VarNotDone String
                    deriving (Eq)

instance Show RuntimeError where
    showsPrec _ (VarNotDone s) = ("Variable " ++) . (s ++) . (" not sufficiently instantiated" ++)
