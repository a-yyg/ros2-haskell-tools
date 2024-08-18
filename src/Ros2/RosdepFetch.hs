module Ros2.RosdepFetch where

import Ros2.PackageParser (PackageParseError)

import Data.ByteString.Char8 hiding (empty)
import Network.HTTP.Client
import Network.HTTP.Simple

import Data.Either (partitionEithers)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void [Char]

whiteSpace :: Parser ()
whiteSpace =
  L.space
    hspace1
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

baseUrl = "https://raw.githubusercontent.com/ros/rosdistro/master/rosdep/base.yaml"
pythonUrl = "https://raw.githubusercontent.com/ros/rosdistro/master/rosdep/python.yaml"
rubyUrl = "https://raw.githubusercontent.com/ros/rosdistro/master/rosdep/ruby.yaml"

fetchFromUrl :: String -> IO ByteString
fetchFromUrl url = do
  request <- parseRequest url
  response <- httpBS request
  return $ getResponseBody response

-- >>> mapM fetchFromUrl [baseUrl, pythonUrl, rubyUrl]

systemDepFile :: Parser [String]
systemDepFile = manyTill systemDep eof

parseSystemDepFile :: String -> Either (ParseErrorBundle String Void) [String]
parseSystemDepFile = parse systemDepFile ""

systemDep :: Parser String
systemDep = do
  name' <- manyTill (anySingleBut ' ') (string ":" >> manyTill anySingle (char '\n'))
  many (string "  " >> manyTill anySingle (char '\n'))
  return name'

-- >>> ((parseSystemDepFile . unpack <$>) . fetchFromUrl) pythonUrl

getSystemDeps :: IO (Either [PackageParseError] [String])
getSystemDeps = do
  deps <- mapM ((parseSystemDepFile . unpack <$>) . fetchFromUrl) [baseUrl, pythonUrl, rubyUrl]
  case partitionEithers deps of
    ([], deps') -> return $ Right $ Prelude.concat deps'
    (errs, _) -> return $ Left errs
