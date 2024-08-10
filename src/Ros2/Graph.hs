module Ros2.Graph where

import Ros2.PackageParser

import qualified Data.Graph as G

-- import Data.Bifunctor

-- | A graph of ROS2 packages and their dependencies
data PkgGraph = PkgGraph
  { packages :: [Package]
  , dependencies :: [(String, String)]
  }
  deriving (Show)

mkPkgGraph :: [Package] -> PkgGraph
mkPkgGraph pkgs = PkgGraph pkgs deps
 where
  getDeps' = getAllDeps . getDepTags
  deps = concatMap (\p -> map (\d -> (Ros2.PackageParser.name p, d)) (getDeps' p)) pkgs

-- | Check the dependencies of a package graph
checkDeps :: PkgGraph -> IO ()
checkDeps = undefined
