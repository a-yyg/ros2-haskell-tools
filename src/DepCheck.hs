{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

-- import Text.XML.HXT.Parser.XmlParsec (xread)

-- import Control.Applicative
-- import System.Console.ArgParser

import Ros2.PackageParser
import System.Console.CmdArgs
import Text.Pretty.Simple

data DepCheck = DepCheck
  { file :: FilePath
  }
  deriving (Show, Data, Typeable)

depChkOpt :: DepCheck
depChkOpt =
  DepCheck
    { file =
        def
          &= typ "FILE"
          &= opt "package.xml"
          &= help "The package.xml file to parse"
    }
    &= summary "depcheck v0.1.0.0"
    &= program "depcheck"

main :: IO ()
main = do
  opts <- cmdArgs depChkOpt
  content <- readFile $ file opts
  -- print content
  case parsePackageXML content of
    Left err -> print err
    Right parsed -> pPrint parsed

-- package <- parsePackageXML content
-- print package
