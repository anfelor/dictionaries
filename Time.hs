{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

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
import           Criterion.Main
import           Criterion.Types
import           System.Directory
import           System.Random

data CliqueCircuitEquality = CliqueCircuitEquality String (Int -> Bool)

data CliqueSubCircuit = CliqueSubCircuit String (Int -> Bool)

main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    [ bgroup
        "Compare a circuit to a clique"
        (cliqueCircuitEquality
          [ CliqueCircuitEquality "Algebra.Graph" (\n -> G.clique [1..n] == G.circuit [1..n])
          , CliqueCircuitEquality "AdjacencyMap" (\n -> AM.clique [1..n] == AM.circuit [1..n])
          , CliqueCircuitEquality "IntAdjacencyMap" (\n -> IAM.clique [1..n] == IAM.circuit [1..n])
          , CliqueCircuitEquality "Relation" (\n -> R.clique [1..n] == R.circuit [1..n])
          ])
    , bgroup
        "Is a clique a subgraph of a circuit?"
        (cliqueSubCircuit
          [ CliqueSubCircuit "Algebra.Graph" (\n -> G.clique [1..n] `G.isSubgraphOf` G.circuit [1..n])
          , CliqueSubCircuit "AdjacencyMap" (\n -> AM.clique [1..n] `AM.isSubgraphOf` AM.circuit [1..n])
          , CliqueSubCircuit "IntAdjacencyMap" (\n -> IAM.clique [1..n] `IAM.isSubgraphOf` IAM.circuit [1..n])
          , CliqueSubCircuit "Relation" (\n -> R.clique [1..n] `R.isSubgraphOf` R.circuit [1..n])
          ])
    ]
  where
    cliqueCircuitEquality funcs =
      [ env (pure ()) (\_ -> bench (title ++ ":" ++ show i) $ nf func i)
      | i <- [100, 1000]
      , CliqueCircuitEquality title func <- funcs
      ]
    cliqueSubCircuit funcs =
      [ env (pure ()) (\_ -> bench (title ++ ":" ++ show i) $ nf func i)
      | i <- [100, 1000]
      , CliqueSubCircuit title func <- funcs
      ]
