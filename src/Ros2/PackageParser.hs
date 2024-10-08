-- {-# LANGUAGE Arrows #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ros2.PackageParser where

-- import Text.Parsec.Prim

-- import Data.Functor.Identity
import Data.Text (Text)
import Data.Void (Void)

-- import Text.Megaparsec.Language (haskellDef)
-- import qualified Text.Megaparsec.Char.Lexer as T
-- import Text.ParserCombinators.Megaparsec
-- import Control.Applicative
-- import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- import Control.Arrow
-- import Text.XML.HXT.Arrow.XmlArrow
-- import Text.XML.HXT.Core
-- import Text.XML.HXT.Parser.XmlParsec as XmlParsec

type Parser = Parsec Void [Char]

whiteSpace :: Parser ()
whiteSpace =
  L.space
    space1
    empty
    (L.skipBlockComment "<!--" "-->")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

type PackageParseError = ParseErrorBundle [Char] Void

data DependencyTags = DependencyTags
  { build_depend :: [String]
  , build_export_depend :: [String]
  , buildtool_depend :: [String]
  , buildtool_export_depend :: [String]
  , exec_depend :: [String]
  , depend :: [String]
  , doc_depend :: [String]
  , test_depend :: [String]
  , conflict :: [String]
  , replace :: [String]
  }
  deriving (Show)

getAllDeps :: DependencyTags -> [String]
getAllDeps deps =
  build_depend deps
    ++ build_export_depend deps
    ++ buildtool_depend deps
    ++ buildtool_export_depend deps
    ++ exec_depend deps
    ++ depend deps
    ++ doc_depend deps
    ++ test_depend deps
    ++ conflict deps
    ++ replace deps

data ExportTags = ExportTags
  { architecture_independent :: Bool
  , build_type :: String
  , deprecated :: Bool
  , message_generator :: String
  , metapackage :: Bool
  }
  deriving (Show)

data GroupDependencies = GroupDependencies
  { group_depend :: [String]
  , member_of_group :: [String]
  }
  deriving (Show)

data Package = Package
  { name :: String
  , version :: String
  , description :: String
  , maintainer :: [String]
  , license :: [String]
  , dependencies :: DependencyTags
  , exports :: ExportTags
  , group_dependencies :: GroupDependencies
  }
  deriving (Show)

getDepTags :: Package -> DependencyTags
getDepTags = dependencies

parsePackageXML :: FilePath -> String -> Either PackageParseError Package
parsePackageXML = parse pkgFile

parsePackageXMLFile :: FilePath -> IO (Either PackageParseError Package)
parsePackageXMLFile file = parsePackageXML file <$> readFile file

pkgFile :: Parser Package
pkgFile = try pkgFile2 <|> pkgFile3

pkgFile2 :: Parser Package
pkgFile2 = do
  optional $ lexeme xmlHeader
  lexeme pkgHeader2
  pkgElements <- pkgBody2
  lexeme pkgFooter
  return $ toPackage pkgElements

pkgFile3 :: Parser Package
pkgFile3 = do
  optional $ lexeme xmlHeader
  lexeme pkgHeader3
  pkgElements <- pkgBody3
  lexeme pkgFooter
  return $ toPackage pkgElements

xmlHeader :: Parser (Maybe String)
xmlHeader = do
  _ <- lexeme $ string "<?xml version=\"1.0\"?>"
  optional
    ( lexeme $
        string "<!DOCTYPE xml>"
    )
  optional
    ( lexeme $
        string "<?xml-model"
          >> manyTill anySingle (try (string "?>"))
    )

pkgHeader2 :: Parser String
pkgHeader2 = string "<package format=\"2\">"

pkgHeader3 :: Parser String
pkgHeader3 = string "<package format=\"3\">"

pkgFooter :: Parser String
pkgFooter = string "</package>"

pkgBody2 :: Parser [PkgElement]
pkgBody2 = many (lexeme pkgElement3)

pkgBody3 :: Parser [PkgElement]
pkgBody3 = many (lexeme pkgElement3)

-- whiteSpace = many (char ' ' <|> char '\t')

data PkgElement
  = PkgName String
  | PkgVersion String
  | PkgDescription String
  | PkgMaintainer String
  | PkgLicense String
  | PkgUrl String
  | PkgAuthor String
  | PkgDependency Dependency
  | PkgExports [Export]
  | PkgGroupDependency GroupDependency

data Dependency
  = BuildDepend String
  | BuildExportDepend String
  | BuildtoolDepend String
  | BuildtoolExportDepend String
  | ExecDepend String
  | Depend String
  | DocDepend String
  | TestDepend String
  | Conflict String
  | Replace String

data Export
  = ArchitectureIndependent
  | BuildType String
  | Deprecated String
  | MessageGenerator String
  | Metapackage

data GroupDependency
  = GroupDepend String
  | MemberOfGroup String

toPackage :: [PkgElement] -> Package
toPackage elements =
  Package
    { name = getName elements
    , version = getVersion elements
    , description = getDescription elements
    , maintainer = getMaintainer elements
    , license = getLicense elements
    , dependencies = getDependencies elements
    , exports = getExports elements
    , group_dependencies = getGroupDependencies elements
    }

getName :: [PkgElement] -> String
getName [] = ""
getName (PkgName x : _) = x
getName (_ : xs) = getName xs

getVersion :: [PkgElement] -> String
getVersion [] = ""
getVersion (PkgVersion x : _) = x
getVersion (_ : xs) = getVersion xs

getDescription :: [PkgElement] -> String
getDescription [] = ""
getDescription (PkgDescription x : _) = x
getDescription (_ : xs) = getDescription xs

getMaintainer :: [PkgElement] -> [String]
getMaintainer [] = []
getMaintainer (PkgMaintainer x : xs) = x : getMaintainer xs
getMaintainer (_ : xs) = getMaintainer xs

getLicense :: [PkgElement] -> [String]
getLicense [] = []
getLicense (PkgLicense x : xs) = x : getLicense xs
getLicense (_ : xs) = getLicense xs

getDependencies :: [PkgElement] -> DependencyTags
getDependencies = foldl getDependency (DependencyTags [] [] [] [] [] [] [] [] [] [])

getDependency :: DependencyTags -> PkgElement -> DependencyTags
getDependency deps (PkgDependency (BuildDepend x)) = deps{build_depend = x : build_depend deps}
getDependency deps (PkgDependency (BuildExportDepend x)) = deps{build_export_depend = x : build_export_depend deps}
getDependency deps (PkgDependency (BuildtoolDepend x)) = deps{buildtool_depend = x : buildtool_depend deps}
getDependency deps (PkgDependency (BuildtoolExportDepend x)) = deps{buildtool_export_depend = x : buildtool_export_depend deps}
getDependency deps (PkgDependency (ExecDepend x)) = deps{exec_depend = x : exec_depend deps}
getDependency deps (PkgDependency (Depend x)) = deps{depend = x : depend deps}
getDependency deps (PkgDependency (DocDepend x)) = deps{doc_depend = x : doc_depend deps}
getDependency deps (PkgDependency (TestDepend x)) = deps{test_depend = x : test_depend deps}
getDependency deps (PkgDependency (Conflict x)) = deps{conflict = x : conflict deps}
getDependency deps (PkgDependency (Replace x)) = deps{replace = x : replace deps}
getDependency deps _ = deps

getExports :: [PkgElement] -> ExportTags
getExports = foldl getExport (ExportTags False [] False "" False)

getExport :: ExportTags -> PkgElement -> ExportTags
getExport exports (PkgExports xs) = foldl getExport' exports xs
getExport exports _ = exports

getExport' :: ExportTags -> Export -> ExportTags
getExport' exports ArchitectureIndependent = exports{architecture_independent = True}
getExport' exports (BuildType x) = exports{build_type = x}
getExport' exports (Deprecated x) = exports{deprecated = True}
getExport' exports (MessageGenerator x) = exports{message_generator = x}
getExport' exports Metapackage = exports{metapackage = True}

getGroupDependencies :: [PkgElement] -> GroupDependencies
getGroupDependencies = foldl getGroupDependency (GroupDependencies [] [])

getGroupDependency :: GroupDependencies -> PkgElement -> GroupDependencies
getGroupDependency deps (PkgGroupDependency (GroupDepend x)) = deps{group_depend = x : group_depend deps}
getGroupDependency deps (PkgGroupDependency (MemberOfGroup x)) = deps{member_of_group = x : member_of_group deps}
getGroupDependency deps _ = deps

pkgElement2 :: Parser PkgElement
pkgElement2 =
  lexeme
    ( try pkgName
        <|> try pkgVersion
        <|> try pkgDescription
        <|> try pkgMaintainer
        <|> try pkgLicense
        <|> try pkgUrl
        <|> try pkgAuthor
        <|> try pkgDependency
        <|> try pkgExport
    )

pkgElement3 :: Parser PkgElement
pkgElement3 =
  lexeme
    ( try pkgName
        <|> try pkgVersion
        <|> try pkgDescription
        <|> try pkgMaintainer
        <|> try pkgLicense
        <|> try pkgUrl
        <|> try pkgAuthor
        <|> try pkgDependency
        <|> try pkgExport
        <|> try pkgGroupDependency
    )

pkgName :: Parser PkgElement
pkgName = do
  string "<name>"
  x <- manyTill anySingle (string "</name>")
  return $ PkgName x

-- >>> parse pkgName "" "<name>ros2_package</name>\n"
-- Right "ros2_package"

pkgVersion :: Parser PkgElement
pkgVersion = do
  string "<version>"
  x <- manyTill anySingle (string "</version>")
  return $ PkgVersion x

-- >>> parse pkgVersion "" "<version>0.1.0</version>\n"
-- Right "0.1.0"

pkgDescription :: Parser PkgElement
pkgDescription = do
  string "<description>"
  x <- manyTill anySingle (string "</description>")
  return $ PkgDescription x

-- >>> parse pkgDescription "" "<description>Package description</description>\n"
-- Right "Package description"

-- pkgMaintainerEmail :: Parser PkgElement
-- pkgMaintainerEmail = do
--   string "<maintainer email=\""
--   email <- manyTill anySingle (string "\">")
--   name <- manyTill anySingle (string "</maintainer>")
--   return $ PkgMaintainer name
--
-- pkgMaintainer :: Parser PkgElement
-- pkgMaintainer = do
--   string "<maintainer>"
--   x <- manyTill anySingle (string "</maintainer>")
--   return $ PkgMaintainer x
pkgMaintainer = parseTagP ["email"] "maintainer" PkgMaintainer

-- >>> parse pkgMaintainer "" "<maintainer email=\"johndoe@gmail.com\">John Doe</maintainer>\n"
-- Right "John Doe"

pkgLicense :: Parser PkgElement
pkgLicense = do
  string "<license>"
  x <- manyTill anySingle (string "</license>")
  return $ PkgLicense x

-- >>> parse pkgLicense "" "<license>MIT</license>\n"
-- Right "MIT"

-- pkgUrl :: Parser PkgElement
-- pkgUrl = do
--   string "<url>"
--   x <- manyTill anySingle (string "</url>")
--   return $ PkgUrl x
pkgUrl = parseTagP ["type"] "url" PkgUrl

-- >>> parse pkgUrl "" "<url>www.github.com</url>\n"
-- Right "www.github.com"

-- pkgAuthorEmail :: Parser PkgElement
-- pkgAuthorEmail = do
--   string "<author email=\"" >> manyTill anySingle (string "\">")
--   x <- manyTill anySingle (string "</author>")
--   return $ PkgAuthor x
--
-- pkgAuthor :: Parser PkgElement
-- pkgAuthor = do
--   string "<author>"
--   x <- manyTill anySingle (string "</author>")
--   return $ PkgAuthor x
pkgAuthor = parseTagP ["email"] "author" PkgAuthor

-- >>> parse pkgAuthor "" "<author>John Doe</author>\n"
-- Right "John Doe"

pkgDependency :: Parser PkgElement
pkgDependency = do
  x <-
    try pkgBuildDepend
      <|> try pkgBuildExportDepend
      <|> try pkgBuildtoolDepend
      <|> try pkgBuildtoolExportDepend
      <|> try pkgExecDepend
      <|> try pkgDepend
      <|> try pkgDocDepend
      <|> try pkgTestDepend
      <|> try pkgConflict
      <|> try pkgReplace
  return $ PkgDependency x

-- parseTagP :: String -> [String] -> (String -> a) -> Parser a
parseTagP opts tag p = do
  lexeme $ string ("<" ++ tag)
  skipMany $ lexeme $ choice $ map tagOption opts
  char '>'
  x <- takeWhile1P Nothing (/= '<')
  lexeme $ string ("</" ++ tag ++ ">")
  return $ p x

tagOption :: String -> Parser (String)
tagOption opt = do
  lexeme $ string (opt ++ "=\"")
  manyTill anySingle (char '"')

-- parseTag tag p = do
--   lexeme $ string ("<" ++ tag ++ ">")
--   x <- takeWhile1P Nothing (/= '<')
--   lexeme $ string ("</" ++ tag ++ ">")
--   return $ p x

parseTag = parseTagP []

pkgBuildDepend = parseTag "build_depend" BuildDepend
pkgBuildExportDepend = parseTag "build_export_depend" BuildExportDepend
pkgBuildtoolDepend = parseTag "buildtool_depend" BuildtoolDepend
pkgBuildtoolExportDepend = parseTag "buildtool_export_depend" BuildtoolExportDepend
pkgExecDepend = parseTag "exec_depend" ExecDepend
pkgDepend = parseTag "depend" Depend
pkgDocDepend = parseTag "doc_depend" DocDepend
pkgTestDepend = parseTag "test_depend" TestDepend
pkgConflict = parseTag "conflict" Conflict
pkgReplace = parseTag "replace" Replace

-- >>> parse pkgDependencies "" "<build_depend>roscpp</build_depend>\n"
-- Right "roscpp"

pkgExport :: Parser PkgElement
pkgExport = do
  lexeme $ string "<export>"
  -- x <-
  --   some
  --     ( try pkgArchitectureIndependent
  --         <|> try pkgBuildType
  --         <|> try pkgDeprecated
  --         <|> try pkgMessageGenerator
  --         <|> try pkgMetapackage
  --     )
  _ <- manyTill anySingle (string "</export>") -- TODO: parse export tags
  -- lexeme $ string "</export>"
  return $ PkgExports []

pkgArchitectureIndependent = do
  lexeme $ string "<architecture_independent/>"
  return ArchitectureIndependent

pkgBuildType = lexeme $ parseTag "build_type" BuildType
pkgDeprecated = do
  lexeme $ string "<deprecated>"
  x <- manyTill anySingle (string "</deprecated>")
  return $ Deprecated x

pkgMessageGenerator = lexeme $ parseTag "message_generator" MessageGenerator
pkgMetapackage = do
  lexeme $ string "<metapackage/>"
  return Metapackage

pkgGroupDependency :: Parser PkgElement
pkgGroupDependency = do
  x <-
    try pkgGroupDepend
      <|> try pkgMemberOfGroup
  return $ PkgGroupDependency x

pkgGroupDepend = lexeme $ parseTag "group_depend" GroupDepend
pkgMemberOfGroup = lexeme $ parseTag "member_of_group" MemberOfGroup
