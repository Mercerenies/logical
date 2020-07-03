
module Language.Logic.Term.Handle where

-- Currently, the only type of handle is a reference handle, which
-- stores a reference to a mutable variable. In the future, this will
-- also be the datatype used for things like stream handles or process
-- identifiers. Basically, anything that I want to be representable in
-- the language in a way entirely opaque to the user will go here.

data Handle = HandleRef Integer
              deriving (Eq, Ord)

instance Show Handle where
    showsPrec _ (HandleRef n) = ("ref(" ++) . shows n . (")" ++)
