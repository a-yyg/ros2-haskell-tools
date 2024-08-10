{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

-- import Text.XML.HXT.Parser.XmlParsec (xread)

-- import Control.Applicative
-- import System.Console.ArgParser

import Ros2.PackageParser
import System.Console.CmdArgs
import Text.Pretty.Simple

data Parser = Parser
  { file :: FilePath
  }
  deriving (Show, Data, Typeable)

parserOpt :: Parser
parserOpt =
  Parser
    { file =
        def
          &= typ "FILE"
          &= opt "package.xml"
          &= help "The package.xml file to parse"
    }
    &= summary "pkgparser v0.1.0.0"
    &= program "pkgparser"

main :: IO ()
main = do
  opts <- cmdArgs parserOpt
  content <- readFile $ file opts
  -- print content
  case parsePackageXML content of
    Left err -> print err
    Right parsed -> pPrint parsed

-- package <- parsePackageXML content
-- print package
