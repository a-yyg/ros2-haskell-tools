{-# LANGUAGE RankNTypes #-}
module Ros2.PrettyError where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data TerminalColor = Red | Green | Yellow | Blue | Magenta | Cyan | White
colored :: TerminalColor -> String -> String
colored color text = "\ESC[" ++ show code ++ "m" ++ text ++ "\ESC[0m"
 where
  code = case color of
    Red -> 91
    Green -> 32
    Yellow -> 33
    Blue -> 34
    Magenta -> 35
    Cyan -> 36
    White -> 37

prettyError ::
  forall s e.
  ( VisualStream s
  , TraversableStream s
  , ShowErrorComponent e
  ) =>
  ParseErrorBundle s e ->
  String
prettyError err = colored Red $ errorBundlePretty err

-- data ExtendedError = PrettyError
--   { file :: String
--   , line :: Int
--   , column :: Int
--   , highlight :: String
--   , message :: String
--   }
--   deriving (Show)

-- data ErrorPrinter f l c h m e = ErrorPrinter
--   { filePrinter :: f -> String
--   , linePrinter :: l -> String
--   , columnPrinter :: c -> String
--   , highlightPrinter :: h -> String
--   , messagePrinter :: m -> String
--   , errorPrinter :: e -> ExtendedError
--   }

-- class ErrorPrinter e where
--   prettyError :: e -> ExtendedError
--
-- instance ErrorPrinter (ParseErrorBundle String String) where
--   prettyError err = PrettyError
--     { file = ""
--     , line = 0
--     , column = 0
--     , highlight = ""
--     , message = errorBundlePretty err
--     }
