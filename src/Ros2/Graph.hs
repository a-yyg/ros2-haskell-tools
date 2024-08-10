module Ros2.Graph where

import Ros2.PackageParser hiding (dependencies)

import Data.Either
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

data DependencyError = DependencyError
  { pkgName :: String
  , missingDeps :: [String]
  }
  deriving (Show)

checkPkg :: [String] -> PkgGraph -> Package -> Either DependencyError ()
checkPkg i g p =
  let
    deps = getAllDeps $ getDepTags p
    missing = filter (\d -> d `notElem` (i ++ map fst (dependencies g))) deps
   in
    if null missing
      then Right ()
      else Left $ DependencyError (name p) missing

-- | Check the dependencies of a package graph
checkDeps :: [String] -> PkgGraph -> Either [DependencyError] ()
checkDeps i g =
  case partitionEithers (map (checkPkg i g) (packages g)) of
    ([], _) -> Right ()
    (errs, _) -> Left errs
