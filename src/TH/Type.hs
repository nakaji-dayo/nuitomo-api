{-# LANGUAGE TemplateHaskell #-}

module TH.Type where
import           Control.Lens
import           Data.Aeson                    as A
import           Data.Default.Class
import           Data.Swagger                  as SW hiding (name)
import           Language.Haskell.TH
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT
import           Text.Casing


customOptions :: Options
customOptions = defaultOptions
  { A.fieldLabelModifier = quietSnake
  , A.constructorTagModifier = quietSnake
  }

deriveApiField :: Name -> Q [Dec]
deriveApiField = _deriveApiField 'customOptions

_deriveApiField :: Name -> Name -> Q [Dec]
_deriveApiField opt n = do
  fs <- makeFieldsNoPrefix n
  return $
    [ InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT n)) [ValD (VarP 'toEncoding) (NormalB (AppE (VarE 'genericToEncoding) (VarE opt))) []]
    , InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT n)) [ValD (VarP 'parseJSON) (NormalB (AppE (VarE 'genericParseJSON) (VarE opt))) []]
    , InstanceD Nothing [] (AppT (ConT ''Default) (ConT n)) []
    -- Swagger
    , InstanceD Nothing [] (AppT (ConT ''ToSchema) (ConT n)) [ValD (VarP 'declareNamedSchema) (NormalB (AppE (VarE 'genericDeclareNamedSchema) (RecUpdE (VarE 'defaultSchemaOptions) [('SW.fieldLabelModifier,AppE (VarE 'A.fieldLabelModifier) (VarE opt))]))) []]
    , InstanceD Nothing [] (AppT (ConT ''Arbitrary) (ConT n)) [ValD (VarP 'arbitrary) (NormalB (VarE 'genericArbitrary)) []]
    ] ++ fs

deriveApiFieldSumType :: Name -> Name -> Q [Dec]
deriveApiFieldSumType n d =
  return
  [  InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT n)) [ValD (VarP 'toEncoding) (NormalB (AppE (VarE 'genericToEncoding) (VarE 'customOptions))) []]
    , InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT n)) [ValD (VarP 'parseJSON) (NormalB (AppE (VarE 'genericParseJSON) (VarE 'customOptions))) []]
    , InstanceD Nothing [] (AppT (ConT ''ToSchema) (ConT n)) [ValD (VarP 'declareNamedSchema) (NormalB (AppE (VarE 'genericDeclareNamedSchema) (RecUpdE (VarE 'defaultSchemaOptions) [('SW.fieldLabelModifier, VarE 'quietSnake), ('SW.constructorTagModifier, VarE 'quietSnake)]))) []]
  , InstanceD Nothing [] (AppT (ConT ''ToParamSchema) (ConT n)) []
  , InstanceD Nothing [] (AppT (ConT ''Arbitrary) (ConT n)) [ValD (VarP 'arbitrary) (NormalB (VarE 'genericArbitrary)) []]
  , InstanceD Nothing [] (AppT (ConT ''Default) (ConT n)) [ValD (VarP 'def) (NormalB (ConE d)) []]
  ]