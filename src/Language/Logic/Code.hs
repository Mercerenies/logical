
module Language.Logic.Code where

import Language.Logic.Term
import qualified Language.Logic.Util as Util

import Data.Map(Map)
import qualified Data.Map as Map

data Clause = Clause Fact [Fact]
              deriving (Show)

-- It would be slightly more efficient to index the map by both name
-- and arity (as opposed to just name), but that's probably
-- insignificant.
data CodeBody = CodeBody (Map String [Clause])

lookupHead :: String -> CodeBody -> [Clause]
lookupHead s (CodeBody m) = maybe [] id $ Map.lookup s m

consolidateClauses :: [Clause] -> CodeBody
consolidateClauses = CodeBody . Util.classify (\(Clause (Fact h _) _) -> h)
