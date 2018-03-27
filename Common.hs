{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as C
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.Relation as R
import qualified Algebra.Graph.Fold as F
import qualified Algebra.Graph.IntAdjacencyMap as IAM
import           Control.DeepSeq
import qualified Data.Tree as Tree

instance NFData (G.Graph a) where
  rnf G.Empty = ()
  rnf (G.Vertex a) = seq a ()
  rnf (G.Overlay a b) = rnf a `seq` (rnf b `seq` ())
  rnf (G.Connect a b) = rnf a `seq` (rnf b `seq` ())

instance NFData (AM.AdjacencyMap a) where
  rnf x = seq x ()

instance NFData IAM.IntAdjacencyMap where
  rnf x = seq x ()

instance NFData (F.Fold a) where
  rnf x = seq x ()

instance Eq a => NFData (R.Relation a) where
  rnf x = (x == x) `seq` ()
  -- TODO: seq x () doesn't lead to full evaluation of the term,
  -- but this doesn't fair either.


-- | A tree with n! + 1 nodes.
-- The nodes are labelled with the natural numbers in a depth-first manner.
someTree :: Int -> Tree.Tree Int
someTree n = fst $ go n 0
  where
    go 0 m = (Tree.Node m [], m+1)
    go n m = let (m', gs) = list n n (m + 1) in (Tree.Node m gs, m')

    list 0 _ m = (m, [])
    list len n m = let (g, m') = go (n - 1) m in (g :) <$> list (len - 1) n m'
