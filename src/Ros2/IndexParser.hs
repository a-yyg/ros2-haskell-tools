module Ros2.IndexParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void [Char]

whiteSpace :: Parser ()
whiteSpace =
  L.space
    hspace1
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

data Repository = Repository
  { name :: String
  , packages :: [String]
  }
  deriving (Show)

parseIndexFile :: String -> Either (ParseErrorBundle String Void) [String]
parseIndexFile = parse indexFile ""

indexFile :: Parser [String]
indexFile = do
  _ <- manyTill anySingle (string "repositories:\n")
  repos <- many repository
  return $ concatMap packages repos ++ map name repos

repository :: Parser Repository
repository = do
  string "  "
  name <- manyTill anySingle (string ":\n")
  optional doc
  pkgs <- optional release
  optional source
  optional $ string "    status: " >> manyTill anySingle (string "\n")
  return $ case pkgs of
    Just p -> Repository name p
    Nothing -> Repository name []

doc = do
  string "    doc:\n"
  string "      type: " >> manyTill anySingle (string "\n")
  string "      url: " >> manyTill anySingle (string "\n")
  string "      version: " >> manyTill anySingle (string "\n")

release :: Parser [String]
release = do
  string "    release:\n"
  pkgs <- optional (string "      packages:\n" >> many package)
  string "      tags:\n"
  string "        release: " >> manyTill anySingle (string "\n")
  string "      url: " >> manyTill anySingle (string "\n")
  optional $ string "      version: " >> manyTill anySingle (string "\n")
  return $ case pkgs of
    Just p -> p
    Nothing -> []

package :: Parser String
package = do
  string "      - " >> manyTill anySingle (string "\n")

source = do
  string "    source:\n"
  optional $ string "      test_commits: " >> manyTill anySingle (string "\n")
  optional $ string "      test_pull_requests: " >> manyTill anySingle (string "\n")
  string "      type: " >> manyTill anySingle (string "\n")
  string "      url: " >> manyTill anySingle (string "\n")
  string "      version: " >> manyTill anySingle (string "\n")
