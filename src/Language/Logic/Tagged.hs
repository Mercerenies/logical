
module Language.Logic.Tagged where

import Data.Hashable

-- Tagged a b behaves like b for all intents and purposes. Two Tagged
-- instances are equal if their second argument is equal, they compare
-- for ordering by comparing their second argument, etc. The first
-- argument is only used when showing a Tagged instance and is
-- intended to be used for debugging purposes. Generally, the type a
-- will be either String or StringId, the latter of which is defined
-- below.
data Tagged a b = Tagged a b

newtype StringId = StringId String
    deriving (Eq, Ord)

instance Show StringId where
    showsPrec _ (StringId s) = (s ++)

instance Eq b => Eq (Tagged a b) where
    Tagged _ b == Tagged _ b' = b == b'

instance Ord b => Ord (Tagged a b) where
    Tagged _ b `compare` Tagged _ b' = b `compare` b'

instance Show a => Show (Tagged a b) where
    showsPrec n (Tagged a _) = showsPrec n a

instance Hashable b => Hashable (Tagged a b) where
    hashWithSalt n (Tagged _ b) = hashWithSalt n b
    hash (Tagged _ b) = hash b
