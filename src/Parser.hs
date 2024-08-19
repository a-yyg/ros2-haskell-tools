{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

-- import Text.XML.HXT.Parser.XmlParsec (xread)

-- import Control.Applicative
-- import System.Console.ArgParser

module Parser (doParse) where

import Ros2.PackageParser (parsePackageXMLFile)
import Ros2.PrettyError
import System.Console.CmdArgs
import Text.Pretty.Simple

-- import Text.Megaparsec (errorBundlePretty)

-- data Parse = Parse
--   { file :: FilePath
--   }
--   deriving (Show, Data, Typeable)

-- parserOpt :: Parser
-- parserOpt =
--   Parser
--     { file =
--         def
--           &= typ "FILE"
--           &= opt "package.xml"
--           &= help "The package.xml file to parse"
--     }
--     &= summary "pkgparser v0.1.0.0"
--     &= program "pkgparser"

doParse :: FilePath -> IO ()
doParse file = do
  r <- parsePackageXMLFile file
  case r of
    Left err -> putStrLn $ prettyError err
    Right parsed -> pPrint parsed

-- package <- parsePackageXML content
-- print package
