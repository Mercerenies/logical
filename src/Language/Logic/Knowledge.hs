
module Language.Logic.Knowledge where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Sequence(Seq(..))
import qualified Data.Sequence as Seq

data KnowledgeBase k a = KnowledgeBase (Map k (Seq a))

empty :: KnowledgeBase k a
empty = KnowledgeBase Map.empty

getFacts :: Ord k => k -> KnowledgeBase k a -> Seq a
getFacts k (KnowledgeBase m) = Map.findWithDefault Seq.empty k m

addFact :: Ord k => k -> a -> KnowledgeBase k a -> KnowledgeBase k a
addFact k a (KnowledgeBase m) = KnowledgeBase (Map.alter go k m)
    where go Nothing = Just $ Seq.singleton a
          go (Just as) = Just $ as :|> a
