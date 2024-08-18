{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import DepCheck (doDeps)
import Parser (doParse)
import Data.Data
import System.Console.CmdArgs

data Rostool
  = Deps
      { dir :: FilePath
      , index :: FilePath
      , verbose :: Bool
      }
  | Parse
      { file :: FilePath
      }
  deriving (Show, Data, Typeable)

depsOpt :: Rostool
depsOpt =
  Deps
    { dir =
        def
          &= typDir
          &= help "The directory to check for dependencies"
    , index =
        def
          &= typFile
          &= help "The ROS2 index index file"
    , verbose =
        def
          &= help "Enable verbose messages"
    }
    &= help "Check dependencies of a ROS2 package"
    -- &= program "rostool deps"
    -- &= summary "rostool deps v0.1.0.0"

parseOpt :: Rostool
parseOpt =
  Parse
    { file =
        def
          &= typFile
          &= help "The package.xml file to parse"
    }
    &= help "Parse a ROS2 package.xml file"
    -- &= program "rostool parse"
    -- &= summary "rostool parse v0.1.0.0"

main :: IO ()
main = do
  args <- cmdArgs (modes [depsOpt &= auto, parseOpt])
  case args of
    Deps {dir = "", index, verbose} -> error "Directory must be specified"
    Deps {dir, index = "", verbose} -> error "Index file must be specified"
    Deps {dir, index, verbose} -> doDeps dir index verbose
    Parse {file} -> doParse file
