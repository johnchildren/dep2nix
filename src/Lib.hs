{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( dep2nix
  ) where

import           Control.Applicative
import           Data.Fix
import qualified Data.HashMap.Lazy   as HashMap
import           Data.Map            as Map
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Nix.Expr
import           Nix.Pretty          (prettyNix)
import           Text.Parsec.Error   (ParseError)
import           Text.Toml           (parseTomlDoc)
import           Text.Toml.Types

newtype ConvertError =
  ConvertParseError ParseError
  deriving (Show)

dep2nix :: T.Text -> Either ConvertError T.Text
dep2nix dep =
  case parseTomlDoc "Gopkg.lock" dep of
    Right table ->
      Right $
      T.pack $
      show $
      prettyNix $ goDeps $ projects table
    Left err -> Left $ ConvertParseError err

data Project = Project
  { _name     :: T.Text
  , _packages :: V.Vector T.Text
  } deriving (Show)

projects :: Table -> V.Vector Project
projects table =
  case HashMap.lookupDefault (VTArray V.empty) "projects" table of
    VTArray vec -> marshalProject <$> vec
    _           -> V.empty -- TODO: should be an error

marshalProject :: Table -> Project
marshalProject table =
  Project {_name = marshalName table, _packages = marshalPackages table}

marshalName :: Table -> T.Text
marshalName table =
  case HashMap.lookupDefault (VString "") "name" table of
    VString name -> name
    _            -> ""

marshalPackages :: Table -> V.Vector T.Text
marshalPackages table =
  case HashMap.lookupDefault (VArray V.empty) "packages" table of
    VArray vec -> marshalPackage <$> vec
    _          -> V.empty
  where
    marshalPackage :: Node -> T.Text
    marshalPackage node =
      case node of
        VString pkg -> pkg
        _           -> "" -- TODO: should be an error

goDeps :: V.Vector Project -> NExpr
goDeps projects = Fix $ NList $ V.toList (goProjectExpr <$> projects)

goProjectExpr :: Project -> NExpr
goProjectExpr project = goPackageExpr (_name project) (_packages project)

goPackageExpr :: T.Text -> V.Vector T.Text -> NExpr
goPackageExpr name packages =
  Fix $
     NSet
       [ stringBind "name" name
       , stringBind "goPackagePath" name
       , stringArrayBind "subPackages" packages
       ]

defaultNix :: T.Text -> T.Text -> Table -> NExpr
defaultNix name path table =
  Fix $
  NAbs
    (ParamSet
       (FixedParamSet
          (Map.fromList [("stdenv", Nothing), ("buildGoPackage", Nothing)]))
       Nothing)
    (Fix $
     NApp
       (Fix $ NSym "buildGoPackage")
       (Fix $
        NRecSet
          [ stringBind "name" name
          , stringBind "goPackagePath" path
          , symbolicBind "src" "./."
          , symbolicBind "goDeps" "./deps.nix"
          , NamedVar [StaticKey "meta"] (Fix $ NSet [])
          ]))

stringBind :: T.Text -> T.Text -> Binding NExpr
stringBind var str =
  NamedVar [StaticKey var] (Fix $ NStr (DoubleQuoted [Plain str]))

symbolicBind :: T.Text -> T.Text -> Binding NExpr
symbolicBind var bind = NamedVar [StaticKey var] (Fix $ NSym bind)

stringArrayBind :: T.Text -> V.Vector T.Text -> Binding NExpr
stringArrayBind var vec =
  NamedVar [StaticKey var] (Fix $ NList $ nString <$> V.toList vec)
  where
    nString str = Fix $ NStr (DoubleQuoted [Plain str])
