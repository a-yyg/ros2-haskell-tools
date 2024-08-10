{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

-- import Text.XML.HXT.Parser.XmlParsec (xread)

-- import Control.Applicative
-- import System.Console.ArgParser

import Ros2.PackageParser
import Ros2.Graph
import System.Console.CmdArgs
import Text.Pretty.Simple

import Data.List
import System.Directory.Recursive
import Text.Parsec (ParseError)
import Data.Either (partitionEithers)
import Control.Monad (filterM)

data DepCheck = DepCheck
  { dir :: FilePath
  }
  deriving (Show, Data, Typeable)

depChkOpt :: DepCheck
depChkOpt =
  DepCheck
    { dir =
        def
          &= typ "DIR"
          &= opt "."
          &= help "The directory to check for dependencies"
    }
    &= summary "depcheck v0.1.0.0"
    &= program "depcheck"

main :: IO ()
main = pPrint =<< parseRootDir . dir =<< cmdArgs depChkOpt
-- main = pPrint =<< getPackageXMLs . dir =<< cmdArgs depChkOpt

parseRootDir :: FilePath -> IO PkgGraph
parseRootDir root = do
  r <- parsePackageXMLs =<< getPackageXMLs root
  case partitionEithers r of
    ([], pkgs) -> return $ mkPkgGraph pkgs
    (errs, _) -> error $ "Error parsing package.xml files: " ++ show errs

getPackageXMLs :: FilePath -> IO [FilePath]
-- getPackageXMLs = fmap (filter ("package.xml" `isSuffixOf`)) . getDirectoryContents
getPackageXMLs files = do
  curFiles <- getDirRecursive files
  return $ filter ("package.xml" `isSuffixOf`) curFiles


parsePackageXMLs :: [FilePath] -> IO [Either ParseError Package]
parsePackageXMLs = fmap (map parsePackageXML) . mapM readFile
