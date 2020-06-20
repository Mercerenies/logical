
module Language.Logic.Error where

import Language.Logic.Term

-- These errors are actual runtime errors, not simple failures which
-- backtrack. Generally, the presence of one of these errors will
-- abort the whole program. Later, we may add some way to "catch"
-- errors.

data RuntimeError = VarsNotDone [String]
                  | TypeError String Term
                  | ArithmeticError String
                    deriving (Eq)

instance Show RuntimeError where
    showsPrec _ (VarsNotDone s) =
        ("Variable(s) " ++) . shows s . (" not sufficiently instantiated" ++)
    showsPrec _ (TypeError exp_ act_) =
        ("Expecting " ++) . (exp_ ++) . (", got " ++) . shows act_
    showsPrec _ (ArithmeticError s) =
        ("Arithmetic error: " ++) . (s ++)
