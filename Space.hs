{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Example uses of comparing map-like data structures.

module Main where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Class as C
import qualified Algebra.Graph.HigherKinded.Class as H
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.Relation as R
import qualified Algebra.Graph.Fold as F
import qualified Algebra.Graph.IntAdjacencyMap as IAM
import           Common
import           Control.DeepSeq
import           Control.Monad
import           System.Random
import           Weigh

-- | Weigh maps.
main :: IO ()
main =
  mainWith
    (do setColumns [Case,Allocated,Max,Live,GCs]
        clique
        tree)

clique :: Weigh ()
clique = forM_ ([10, 100] :: [Int]) $ \n ->
  do let !name = "clique [1.." ++ show n ++ "] as "
     func (name ++ "Algebra.Graph")   (\n -> G.clique [1..n]) n
     func (name ++ "AdjacencyMap")    (\n -> AM.clique [1..n]) n
     func (name ++ "IntAdjacencyMap") (\n -> IAM.clique [1..n]) n
     func (name ++ "Relation")        (\n -> R.clique [1..n]) n

-- | Storage of the tree in someTree. n! + 1 vertices and n! edges,
-- so the numbers below are already quite high ;)
tree :: Weigh ()
tree = forM_([5, 8] :: [Int]) $ \n ->
  do let !name = "someTree " ++ show n ++ " as "
     let !tree = someTree n
     func (name ++ "Algebra.Graph") (\n -> G.tree tree :: G.Graph Int) n
     func (name ++ "AdjacencyMap") (\n -> AM.tree tree :: AM.AdjacencyMap Int) n
     func (name ++ "IntAdjacencyMap") (\n -> IAM.tree tree :: IAM.IntAdjacencyMap) n
     func (name ++ "Relation") (\n -> R.tree tree :: R.Relation Int) n
