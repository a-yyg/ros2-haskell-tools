{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

-- import Text.XML.HXT.Parser.XmlParsec (xread)

-- import Control.Applicative
-- import System.Console.ArgParser

import Ros2.PackageParser
import System.Console.CmdArgs
import Text.Pretty.Simple

data Options = Options
  { file :: FilePath
  }
  deriving (Show, Data, Typeable)

options :: Options
options =
  Options
    { file =
        def
          &= typ "FILE"
          &= opt "package.xml"
          &= help "The package.xml file to parse"
    }

main :: IO ()
main = do
  opts <- cmdArgs options
  content <- readFile $ file opts
  print content
  pPrint $ parsePackageXML content

-- package <- parsePackageXML content
-- print package
