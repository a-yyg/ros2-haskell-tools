{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

-- import Text.XML.HXT.Parser.XmlParsec (xread)

-- import Control.Applicative
-- import System.Console.ArgParser

import Ros2.Graph
import Ros2.PackageParser
import Ros2.IndexParser
import System.Console.CmdArgs
import Text.Pretty.Simple

import Control.Monad (filterM)
import Data.Either (partitionEithers)
import Data.List
import System.Directory.Recursive
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

data DepCheck = DepCheck
  { dir :: FilePath
  , index :: FilePath
  , systemIndex :: FilePath
  , verbose :: Bool
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
    , index =
        def
          &= typ "FILE"
          &= opt "distribution.yaml"
          &= help "The ROS2 index index file"
    , systemIndex =
        def
          &= typ "FILE"
          &= opt "base.yaml"
          &= help "The system dependencies file"
    , verbose =
        def
          &= help "Enable verbose messages"
    }
    &= summary "depcheck v0.1.0.0"
    &= program "depcheck"

main :: IO ()
main = do
  args' <- cmdArgs depChkOpt
  r <- parseRootDir $ dir args'
  index' <- parseIndexFile <$> readFile (index args')
  systemIndex' <- parseSystemDepFile <$> readFile (systemIndex args')
  case r of
    Left errs -> (if verbose args' then pPrint errs else putStrLn "Error parsing package.xml files")
    Right pkgs -> case (index',systemIndex') of
      (Left err, _) -> putStrLn $ "Error parsing index file: " ++ errorBundlePretty err
      (_, Left err) -> putStrLn $ "Error parsing system index file: " ++ errorBundlePretty err
      (Right i', Right s') -> case checkDeps (i' ++ s') pkgs of
        Left errs -> (if verbose args' then pPrint errs else putStrLn "Dependency errors found")
        Right _ -> putStrLn "No dependency errors found"

-- main = pPrint =<< getPackageXMLs . dir =<< cmdArgs depChkOpt

parseRootDir :: FilePath -> IO (Either [PackageParseError] PkgGraph)
parseRootDir root = do
  xmls <- getPackageXMLs root
  r <- parsePackageXMLs xmls
  let (errs, pkgs) = partitionEithers $ map snd r
  mapM (\(f, e) -> do
    case e of
      Left err -> putStrLn $ f ++ ":\n\ESC[91mError:" ++ errorBundlePretty err ++ "\ESC[0m\n"
      Right pkg -> return ()
    ) r
  return $ if null errs then Right $ mkPkgGraph pkgs else Left errs

      -- putStrLn $
      --   "Error parsing package.xml files: \n"
      --     ++ concatMap (\(xml, err) -> xml ++ ":\n\ESC[91mError:" ++ err ++ "\ESC[0m\n") (zip xmls' (map errorBundlePretty errs))
      --
getPackageXMLs :: FilePath -> IO [FilePath]
-- getPackageXMLs = fmap (filter ("package.xml" `isSuffixOf`)) . getDirectoryContents
getPackageXMLs files = do
  curFiles <- getDirRecursive files
  return $ filter ("package.xml" `isSuffixOf`) curFiles

-- parsePackageXMLs :: [FilePath] -> IO [Either PackageParseError Package]
-- parsePackageXMLs = fmap (map parsePackageXML) . mapM readFile

parsePackageXMLs :: [FilePath] -> IO [(FilePath, Either PackageParseError Package)]
parsePackageXMLs =
  mapM
    ( \f -> do
        fcontent <- readFile f
        let r = parsePackageXML fcontent
        return (f, r)
    )
