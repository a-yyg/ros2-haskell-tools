{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Data
import DepCheck (doDeps)
import Parser (doParse)
import System.Console.CmdArgs

-- https://stackoverflow.com/questions/65483488/mandatory-arguments-with-cmdargs
helpAndExit :: (Data a) => String -> a -> IO ()
helpAndExit msg args =
  do
    let cm = cmdArgsMode args
    let ht =
          msg
            ++ "\n\n"
            ++ show cm
    cmdArgsApply
      ( CmdArgs
          { cmdArgsValue = args
          , cmdArgsHelp = Just ht
          , cmdArgsVersion = Nothing
          , cmdArgsVerbosity = Nothing
          }
      )
    return ()

data Rostool
  = Deps
      { dir :: FilePath
      , distro :: String
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
          &= name "d"
          &= help "The directory to check for dependencies"
    , distro =
        def
          &= typ "DISTRO"
          &= name "D"
          &= help "The ROS2 distribution to check against"
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
    Deps{dir = "", ..} -> helpAndExit "Directory must be specified" depsOpt
    Deps{distro = "", ..} -> helpAndExit "Distribution must be specified" depsOpt
    Deps{dir, distro, verbose} -> doDeps dir distro verbose
    Parse{file = ""} -> helpAndExit "File must be specified" parseOpt
    Parse{file} -> doParse file
