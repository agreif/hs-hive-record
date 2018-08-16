{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Casing as TC
import Data.Aeson
import qualified Data.Maybe as M
import qualified Data.List as L
import qualified Text.Ginger as G
import qualified System.IO as SIO (IOMode(ReadMode), hSetEncoding, withFile, utf8_bom)
import qualified System.IO.Strict as SIOS
import qualified System.IO.Error as SIOE
import qualified Options.Applicative as O
import Data.Semigroup ((<>))
import qualified GHC.IO.Encoding as E

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  templFile <- O.execParser argInfo
  template <- loadTemplate templFile
  putStrLn . Text.unpack $ G.easyRender context template

-- command line parser

argParser :: O.Parser String
argParser = O.argument O.str (O.metavar "TEMPLATE" <> O.help "Ginger template file")

argInfo :: O.ParserInfo String
argInfo = O.info (argParser O.<**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Generate code from TEMPLATE"
  <> O.header "hs-generator - yesod partioal code generator" )

-- ginger

loadTemplate :: String -> IO (G.Template G.SourcePos)
loadTemplate templFile = do
  eitherTemplate <- G.parseGingerFile' opts templFile
  return $ case eitherTemplate of
             Left err -> error . show $ err
             Right template' -> template'

opts :: G.ParserOptions IO
opts = (G.mkParserOptions fileResolver) { G.poSourceName = Nothing
                                        , G.poKeepTrailingNewline = True }

fileResolver :: G.IncludeResolver IO
fileResolver filename = do
  content <- loadFile filename
  return $ Just content

loadFile :: FilePath -> IO String
loadFile fn =
  SIOE.tryIOError (loadFile' $ "ginger/" ++ fn) >>= \e ->
    case e of
      Right contents -> return contents
      Left err -> return $ show err
  where
    loadFile' :: FilePath -> IO String
    loadFile' fn' = do
      SIO.withFile fn' SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        contents <- SIOS.hGetContents h
        return contents

-- helpers

upperFirst :: Text -> Text
upperFirst t = Text.append (Text.toUpper $ Text.take 1 t) (Text.drop 1 t)

lowerFirst :: Text -> Text
lowerFirst t = Text.append (Text.toLower $ Text.take 1 t) (Text.drop 1 t)

-- types

data BContext = BContext
  { bContextModels :: [BModel]
  , bContextTranslations :: [BTranslation]
  }

instance ToJSON BContext where
  toJSON o = object $
    [ "models" .= bContextModels o
    , "translations" .= bContextTranslations o
    ] ++ (map (\bModel@(BModel {bModelName = modelName}) -> (modelName <> "Model") .= bModel) $ bContextModels o)

data BModel = BModel
  { bModelName :: Text
  , bModelLabel :: Text
  , bModelIsJson :: Bool
  , bModelDbUniquenesses :: [Text]
  , bModelDbHasHistoryTable :: Bool
  , bModelHsDerivings :: [Text]
  , bModelFields :: [BField]
  , bModelAddFormEntityLoader :: Maybe Text
  , bModelEditFormEntityLoader :: Maybe Text
  , bModelDeleteFormEntityLoader :: Maybe Text
  , bModelAddFormDataJsonUrl :: Maybe Text
  , bModelEditFormDataJsonUrl :: Maybe Text
  , bModelDeleteFormDataJsonUrl :: Maybe Text
  , bModelAddFormHasDefaultModel :: Bool
  , bModelEditPostLoadsModel :: Bool
  , bModelDeletePostLoadsModel :: Bool
  , bModelAddFormTitleMsg :: Maybe Text
  , bModelEditFormTitleMsg :: Maybe Text
  , bModelDeleteFormTitleMsg :: Maybe Text
  , bModelParentHsType :: Maybe Text
  , bModelFormRouteHsType :: Text
  }

instance ToJSON BModel where
  toJSON o = object
    [ "name" .= bModelName o
    , "nameCap" .= (upperFirst $ bModelName o)
    , "label" .= bModelLabel o
    , "isJson" .= bModelIsJson o
    , "dbUniquenesses" .= bModelDbUniquenesses o
    , "dbHasHistoryTable" .= bModelDbHasHistoryTable o
    , "dbTableName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bModelName o))
    , "dbHistoryTableName" .= ((TC.toQuietSnake $ TC.fromAny (Text.unpack $ bModelName o)) ++ "_history")
    , "dbFields" .= getDbFields o
    , "dbUpdatableFields" .= (filter (\field -> case bFieldDb field of
                                                  Just BFieldDb {bFieldDbCanUpdate = canUpdate} -> canUpdate && (M.isJust $ bFieldEditView field)
                                                  Nothing -> False
                                     ) $ bModelFields o)
    , "hsAddAssignmentLines" .= getAddAssignmentLines o
    , "hsDerivings" .= bModelHsDerivings o
    , "fields" .= bModelFields o
    , "addViewFields" .= (filter (\field -> M.isJust $ bFieldAddView field) $ bModelFields o)
    , "editViewFields" .= (filter (\field -> M.isJust $ bFieldEditView field) $ bModelFields o)
    , "isInDb" .= (L.any M.isJust $ L.map bFieldDb $ bModelFields o)
    , "addFormEntityLoader" .= bModelAddFormEntityLoader o
    , "editFormEntityLoader" .= bModelEditFormEntityLoader o
    , "deleteFormEntityLoader" .= bModelDeleteFormEntityLoader o
    , "addFormDataJsonUrl" .= bModelAddFormDataJsonUrl o
    , "editFormDataJsonUrl" .= bModelEditFormDataJsonUrl o
    , "deleteFormDataJsonUrl" .= bModelDeleteFormDataJsonUrl o
    , "addFormHasDefaultModel" .= bModelAddFormHasDefaultModel o
    , "editPostLoadsModel" .= bModelEditPostLoadsModel o
    , "deletePostLoadsModel" .= bModelDeletePostLoadsModel o
    , "addFormTitleMsg" .= bModelAddFormTitleMsg o
    , "editFormTitleMsg" .= bModelEditFormTitleMsg o
    , "deleteFormTitleMsg" .= bModelDeleteFormTitleMsg o
    , "parentHsType" .= bModelParentHsType o
    , "formRouteHsType" .= bModelFormRouteHsType o
    , "parentHsParamId" .= getParentHsParamId o
    , "formHasProgressBar" .= (any (\field -> bFieldHsType field == "FileInfo") $ bModelFields o)
    ]

getDbFields :: BModel -> [BField]
getDbFields m = filter (M.isJust . bFieldDb) $ bModelFields m

getAddAssignmentLines :: BModel -> [Text]
getAddAssignmentLines m =
  (if M.isJust $ bModelParentHsType m then [ Text.concat [bModelName m, M.fromJust $ bModelParentHsType m, "Id", " = ", getParentHsParamId m]] else [])
  ++
  ( map (\f -> Text.concat [bModelName m, upperFirst $ bFieldName f, " = ",
                            case bFieldAddView f of
                              Just _ -> Text.concat ["vAdd", upperFirst $ bModelName m, upperFirst $ bFieldName f, " vAdd", upperFirst $ bModelName m]
                              _ -> "Nothing"
                           ]
        )
    $ filter (\f -> case bModelParentHsType m of
                 Just hsType -> Text.concat [hsType, "Id"] /= bFieldHsType f
                 _ -> True
             )
    $ getDbFields m
  )


getParentHsParamId :: BModel -> Text
getParentHsParamId m = case bModelParentHsType m of
                         Just parentHsType -> lowerFirst $ Text.append parentHsType "Id"
                         _ -> ""


data BTranslation = BTranslation
  { bTranslationKey :: Text
  , bTranslationDe :: Text
  , bTranslationEn :: Text
  }

instance ToJSON BTranslation where
  toJSON o = object
    [ "key" .= bTranslationKey o
    , "keyCap" .= (upperFirst $ bTranslationKey o)
    , "de" .= bTranslationDe o
    , "en" .= bTranslationEn o
    ]


data BFieldDb = BFieldDb
  { bFieldDbIsNullable :: Bool
  , bFieldDbDefault :: Maybe Text
  , bFieldDbCanUpdate :: Bool
  }

instance ToJSON BFieldDb where
  toJSON o = object
    [ "isNullable" .= bFieldDbIsNullable o
    , "isNotNullable" .= (not $ bFieldDbIsNullable o)
    , "default" .= bFieldDbDefault o
    , "canUpdate" .= bFieldDbCanUpdate o
    ]

data BFieldAddView = BFieldAddView
  { bFieldAddViewIsRequired :: Bool
  , bFieldAddViewIsDisabled :: Bool
  , bFieldAddViewAttrs :: [BFieldAttr]
  , bFieldAddViewDefault :: Maybe Text
  }

instance ToJSON BFieldAddView where
  toJSON o = object
    [ "isRequired" .= bFieldAddViewIsRequired o
    , "isOptional" .= (not $ bFieldAddViewIsRequired o)
    , "isDisabled" .= bFieldAddViewIsDisabled o
    , "isEnabled" .= (not $ bFieldAddViewIsDisabled o)
    , "attrs" .= ((if bFieldAddViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                   ++ bFieldAddViewAttrs o)
    , "default" .= bFieldAddViewDefault o
    ]

data BFieldEditView = BFieldEditView
  { bFieldEditViewIsRequired :: Bool
  , bFieldEditViewIsDisabled :: Bool
  , bFieldEditViewAttrs :: [BFieldAttr]
  , bFieldEditViewDefault :: Maybe Text
  }

instance ToJSON BFieldEditView where
  toJSON o = object
    [ "isRequired" .= bFieldEditViewIsRequired o
    , "isOptional" .= (not $ bFieldEditViewIsRequired o)
    , "isDisabled" .= bFieldEditViewIsDisabled o
    , "isEnabled" .= (not $ bFieldEditViewIsDisabled o)
    , "attrs" .= ((if bFieldEditViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                   ++ bFieldEditViewAttrs o)
    , "default" .= bFieldEditViewDefault o
    ]

data BField = BField
  { bFieldName :: Text
  , bFieldLabelDe :: Maybe Text
  , bFieldLabelEn :: Maybe Text
  , bFieldHsType :: Text
  , bFieldDb :: Maybe BFieldDb
  , bFieldFormFieldType :: Maybe Text
  , bFieldAddView :: Maybe BFieldAddView
  , bFieldEditView :: Maybe BFieldEditView
  }

instance ToJSON BField where
  toJSON o = object
    [ "name" .= bFieldName o
    , "nameCap" .= (upperFirst $ bFieldName o)
    , "dbColumnName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bFieldName o))
    , "labelDe" .= bFieldLabelDe o
    , "labelEn" .= bFieldLabelEn o
    , "hsType" .= bFieldHsType o
    , "db" .= bFieldDb o
    , "formFieldType" .= bFieldFormFieldType o
    , "addView" .= bFieldAddView o
    , "editView" .= bFieldEditView o
    , "isHsTypeBool" .= (bFieldHsType o == "Bool")
    , "isForeignKey" .= ((Text.takeEnd 2 $ bFieldName o) == "Id")
    ]

data BFieldAttr = BFieldAttr
  { bFieldAttrKey :: Text
  , bFieldAttrValue :: Text
  }

instance ToJSON BFieldAttr where
  toJSON o = object
    [ "key" .= bFieldAttrKey o
    , "value" .= bFieldAttrValue o
    ]

-- model context

context :: Value
context =
  toJSON $
  BContext
  { bContextModels =
      [ BModel
        { bModelName = "user"
        , bModelLabel = "User"
        , bModelIsJson = False
        , bModelDbUniquenesses = ["UniqueUserIdent ident"]
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Nothing
        , bModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalAddUser"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditUser"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteUser"
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
        , bModelFields =
            [ BField
              { bFieldName = "ident"
              , bFieldLabelDe = Just "Login"
              , bFieldLabelEn = Just "Login"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "password"
              , bFieldLabelDe = Just "Passwort"
              , bFieldLabelEn = Just "Password"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "email"
              , bFieldLabelDe = Just "Email"
              , bFieldLabelEn = Just "Email"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "isAdmin"
              , bFieldLabelDe = Just "Ist Admin?"
              , bFieldLabelEn = Just "Is admin?"
              , bFieldHsType = "Bool"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "checkBoxField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "isResetPassword"
              , bFieldLabelDe = Just "Neues Passwort generieren? (Wird per Email zugesendet)"
              , bFieldLabelEn = Just "Generate new password? (Will be sent by email)"
              , bFieldHsType = "Bool"
              , bFieldDb = Nothing
              , bFieldFormFieldType = Just "checkBoxField"
              , bFieldAddView = Nothing
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Just "Nothing"
                  }
              }
            ]
        }
      , BModel
        { bModelName = "config"
        , bModelLabel = "Config"
        , bModelIsJson = True
        , bModelDbUniquenesses = ["UniqueConfigCode code"]
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Nothing
        , bModelEditFormTitleMsg = Just "MsgGlobalEditConfig"
        , bModelDeleteFormTitleMsg = Nothing
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
        , bModelFields =
            [ BField
              { bFieldName = "code"
              , bFieldLabelDe = Just "Code"
              , bFieldLabelEn = Just "Code"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView = Nothing
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = True
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "stringValue"
              , bFieldLabelDe = Just "String-Wert"
              , bFieldLabelEn = Just "String-Value"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "intValue"
              , bFieldLabelDe = Just "Integer-Wert"
              , bFieldLabelEn = Just "Integer-Value"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "doubleValue"
              , bFieldLabelDe = Just "Double-Wert"
              , bFieldLabelEn = Just "Double-Value"
              , bFieldHsType = "Double"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "doubleField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "boolValue"
              , bFieldLabelDe = Just "Boolean-Wert"
              , bFieldLabelEn = Just "Boolean-Value"
              , bFieldHsType = "Bool"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "checkBoxField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }
      , BModel
        { bModelName = "testmail"
        , bModelLabel = "Test Mail"
        , bModelIsJson = False
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = False
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Just "MyprojectR TestMailDataJsonR"
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalSendTestMail"
        , bModelEditFormTitleMsg = Nothing
        , bModelDeleteFormTitleMsg = Nothing
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
        , bModelFields =
            [ BField
              { bFieldName = "email"
              , bFieldLabelDe = Just "Email"
              , bFieldLabelEn = Just "Email"
              , bFieldHsType = "Text"
              , bFieldDb = Nothing
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView = Nothing
              }
            ]
        }



      , BModel
        { bModelName = "Rawdata"
        , bModelLabel = "Rawdata"
        , bModelIsJson = False
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = False
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Nothing
        , bModelEditFormDataJsonUrl = Nothing
        , bModelDeleteFormDataJsonUrl = Nothing
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Nothing
        , bModelEditFormTitleMsg = Nothing
        , bModelDeleteFormTitleMsg = Nothing
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "CrmR"
        , bModelFields =
            [ BField
              { bFieldName = "bytes"
              , bFieldLabelDe = Just "Bytes"
              , bFieldLabelEn = Just "Bytes"
              , bFieldHsType = "ByteString"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            ]
        }



      , BModel
        { bModelName = "location"
        , bModelLabel = "Location"
        , bModelIsJson = True
        , bModelDbUniquenesses = ["UniqueLocationName name"]
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "HiverecR LocationListPageDataJsonR"
        , bModelEditFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataJsonR locationId"
        , bModelDeleteFormDataJsonUrl = Just "HiverecR LocationListPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalAddLocation"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditLocation"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteLocation"
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "HiverecR"
        , bModelFields =
            [ BField
              { bFieldName = "name"
              , bFieldLabelDe = Just "Name"
              , bFieldLabelEn = Just "Name"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }



      , BModel
        { bModelName = "hive"
        , bModelLabel = "Hive"
        , bModelIsJson = True
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataJsonR locationId"
        , bModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR hiveId"
        , bModelDeleteFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataJsonR $ hiveLocationId hive"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = True
        , bModelAddFormTitleMsg = Just "MsgGlobalAddHive"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditHive"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteHive"
        , bModelParentHsType = Just "Location"
        , bModelFormRouteHsType = "HiverecR"
        , bModelFields =
            [ BField
              { bFieldName = "locationId"
              , bFieldLabelDe = Nothing
              , bFieldLabelEn = Nothing
              , bFieldHsType = "LocationId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "name"
              , bFieldLabelDe = Just "Name"
              , bFieldLabelEn = Just "Name"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "description"
              , bFieldLabelDe = Just "Beschreibung"
              , bFieldLabelEn = Just "Description"
              , bFieldHsType = "Textarea"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textareaField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-textarea uk-form-small"
                        }
                      , BFieldAttr
                        {bFieldAttrKey = "rows", bFieldAttrValue = "5"}
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-textarea uk-form-small"
                        }
                      , BFieldAttr
                        {bFieldAttrKey = "rows", bFieldAttrValue = "5"}
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }



      , BModel
        { bModelName = "inspection"
        , bModelLabel = "Inspection"
        , bModelIsJson = True
        , bModelDbUniquenesses = ["UniqueInspectionHiveDate hiveId date"]
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bModelAddFormHasDefaultModel = True
        , bModelEditPostLoadsModel = True
        , bModelDeletePostLoadsModel = True
        , bModelAddFormTitleMsg = Just "MsgGlobalAddInspection"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditInspection"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteInspection"
        , bModelParentHsType = Just "Hive"
        , bModelFormRouteHsType = "HiverecR"
        , bModelFields =
            [ BField
              { bFieldName = "hiveId"
              , bFieldLabelDe = Nothing
              , bFieldLabelEn = Nothing
              , bFieldHsType = "HiveId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "date"
              , bFieldLabelDe = Just "Datum"
              , bFieldLabelEn = Just "Date"
              , bFieldHsType = "Day"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "dayField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs = []
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "temperTypeId"
              , bFieldLabelDe = Just "Sanftmut"
              , bFieldLabelEn = Just "Temper"
              , bFieldHsType = "TemperTypeId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "temperTypeSelectField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs = []
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "runningTypeId"
              , bFieldLabelDe = Just "Wabensitz"
              , bFieldLabelEn = Just "Running Beh."
              , bFieldHsType = "RunningTypeId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "runningTypeSelectField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs = []
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "swarmingTypeId"
              , bFieldLabelDe = Just "Schwarmtrieb"
              , bFieldLabelEn = Just "swarming Mood"
              , bFieldHsType = "SwarmingTypeId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "swarmingTypeSelectField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs = []
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "queenSeen"
              , bFieldLabelDe = Just "Kö ges."
              , bFieldLabelEn = Just "Queen seen"
              , bFieldHsType = "Bool"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "checkBoxField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "totalFrames"
              , bFieldLabelDe = Just "Ges. Waben"
              , bFieldLabelEn = Just "Total frames"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "beeCoveredFrames"
              , bFieldLabelDe = Just "Bel. Waben"
              , bFieldLabelEn = Just "Bee covered frames"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "broodFrames"
              , bFieldLabelDe = Just "Brutwaben"
              , bFieldLabelEn = Just "Brood frames"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "honeyFrames"
              , bFieldLabelDe = Just "Honigwaben"
              , bFieldLabelEn = Just "Honey frames"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "treatment"
              , bFieldLabelDe = Just "Behandlung"
              , bFieldLabelEn = Just "Treatment"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "feeding"
              , bFieldLabelDe = Just "Fütterung"
              , bFieldLabelEn = Just "Feeding"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "notes"
              , bFieldLabelDe = Just "Notizen"
              , bFieldLabelEn = Just "Notes"
              , bFieldHsType = "Textarea"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = True
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textareaField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = False
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = False
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }
      , BModel
        { bModelName = "inspectionfile"
        , bModelLabel = "Inspection File"
        , bModelIsJson = True
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalAddInspectionfile"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditInspectionfile"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteInspectionfile"
        , bModelParentHsType = Just "Inspection"
        , bModelFormRouteHsType = "HiverecR"
        , bModelFields =
            [ BField
              { bFieldName = "inspectionId"
              , bFieldLabelDe = Nothing
              , bFieldLabelEn = Nothing
              , bFieldHsType = "InspectionId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "rawdataId"
              , bFieldLabelDe = Nothing
              , bFieldLabelEn = Nothing
              , bFieldHsType = "RawdataId"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "filename"
              , bFieldLabelDe = Just "Dateiname"
              , bFieldLabelEn = Just "Filename"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "mimetype"
              , bFieldLabelDe = Just "MIME Type"
              , bFieldLabelEn = Just "MIME Type"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "size"
              , bFieldLabelDe = Just "Groesse"
              , bFieldLabelEn = Just "Size"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = False
                  }
              , bFieldFormFieldType = Nothing
              , bFieldAddView = Nothing
              , bFieldEditView = Nothing
              }
            , BField
              { bFieldName = "file"
              , bFieldLabelDe = Just "Datei"
              , bFieldLabelEn = Just "File"
              , bFieldHsType = "FileInfo"
              , bFieldDb = Nothing
              , bFieldFormFieldType = Just "fileField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs = []
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs = []
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }












      , BModel
        { bModelName = "temperType"
        , bModelLabel = "Temper Type"
        , bModelIsJson = True
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalAddTemperType"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditTemperType"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteTemperType"
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
        , bModelFields =
            [ BField
              { bFieldName = "name"
              , bFieldLabelDe = Just "Name"
              , bFieldLabelEn = Just "Name"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "sortIndex"
              , bFieldLabelDe = Just "Sortierungs-Index"
              , bFieldLabelEn = Just "Sort Index"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }
      , BModel
        { bModelName = "runningType"
        , bModelLabel = "Running Type"
        , bModelIsJson = True
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalAddRunningType"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditRunningType"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteRunningType"
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
        , bModelFields =
            [ BField
              { bFieldName = "name"
              , bFieldLabelDe = Just "Name"
              , bFieldLabelEn = Just "Name"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "sortIndex"
              , bFieldLabelDe = Just "Sortierungs-Index"
              , bFieldLabelEn = Just "Sort Index"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }
      , BModel
        { bModelName = "swarmingType"
        , bModelLabel = "Swarming Type"
        , bModelIsJson = True
        , bModelDbUniquenesses = []
        , bModelDbHasHistoryTable = True
        , bModelHsDerivings = []
        , bModelAddFormEntityLoader = Nothing
        , bModelEditFormEntityLoader = Nothing
        , bModelDeleteFormEntityLoader = Nothing
        , bModelAddFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bModelAddFormHasDefaultModel = False
        , bModelEditPostLoadsModel = False
        , bModelDeletePostLoadsModel = False
        , bModelAddFormTitleMsg = Just "MsgGlobalAddSwarmingType"
        , bModelEditFormTitleMsg = Just "MsgGlobalEditSwarmingType"
        , bModelDeleteFormTitleMsg = Just "MsgGlobalDeleteSwarmingType"
        , bModelParentHsType = Nothing
        , bModelFormRouteHsType = "AdminR"
        , bModelFields =
            [ BField
              { bFieldName = "name"
              , bFieldLabelDe = Just "Name"
              , bFieldLabelEn = Just "Name"
              , bFieldHsType = "Text"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "textField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-large uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            , BField
              { bFieldName = "sortIndex"
              , bFieldLabelDe = Just "Sortierungs-Index"
              , bFieldLabelEn = Just "Sort Index"
              , bFieldHsType = "Int"
              , bFieldDb =
                  Just $
                  BFieldDb
                  { bFieldDbIsNullable = False
                  , bFieldDbDefault = Nothing
                  , bFieldDbCanUpdate = True
                  }
              , bFieldFormFieldType = Just "intField"
              , bFieldAddView =
                  Just $
                  BFieldAddView
                  { bFieldAddViewIsRequired = True
                  , bFieldAddViewIsDisabled = False
                  , bFieldAddViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldAddViewDefault = Nothing
                  }
              , bFieldEditView =
                  Just $
                  BFieldEditView
                  { bFieldEditViewIsRequired = True
                  , bFieldEditViewIsDisabled = False
                  , bFieldEditViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-form-width-medium uk-input uk-form-small"
                        }
                      ]
                  , bFieldEditViewDefault = Nothing
                  }
              }
            ]
        }













      ]
  , bContextTranslations =
    [ BTranslation { bTranslationKey = "home", bTranslationDe = "Home", bTranslationEn = "Home" }
    , BTranslation { bTranslationKey = "admin", bTranslationDe = "Admin", bTranslationEn = "Admin" }
    , BTranslation { bTranslationKey = "logout", bTranslationDe = "Logout", bTranslationEn = "Logout" }
    , BTranslation { bTranslationKey = "language", bTranslationDe = "Sprache", bTranslationEn = "Language" }
    , BTranslation { bTranslationKey = "myProfile", bTranslationDe = "Mein Profil", bTranslationEn = "My Profile" }
    , BTranslation { bTranslationKey = "editMyProfile", bTranslationDe = "Mein Profil bearbeiten", bTranslationEn = "Edit my profile" }
    , BTranslation { bTranslationKey = "reallyDelete", bTranslationDe = "Möchten sie wirklich löschen?", bTranslationEn = "Are you sure to delete?" }
    , BTranslation { bTranslationKey = "users", bTranslationDe = "Nutzer", bTranslationEn = "Users" }
    , BTranslation { bTranslationKey = "addUser", bTranslationDe = "Nutzer hinzufügen", bTranslationEn = "Add user" }
    , BTranslation { bTranslationKey = "editUser", bTranslationDe = "Nutzer bearbeiten", bTranslationEn = "Edit user" }
    , BTranslation { bTranslationKey = "deleteUser", bTranslationDe = "Nutzer löschen", bTranslationEn = "Delete user" }
    , BTranslation { bTranslationKey = "configurations", bTranslationDe = "Konfigurationen", bTranslationEn = "Configurations" }
    , BTranslation { bTranslationKey = "editConfig", bTranslationDe = "Konfiguration bearbeiten", bTranslationEn = "Edit config" }
    , BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
    , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
    , BTranslation { bTranslationKey = "cancel", bTranslationDe = "Abbrechen", bTranslationEn = "Cancel" }


    , BTranslation { bTranslationKey = "location", bTranslationDe = "Standort", bTranslationEn = "Location" }
    , BTranslation { bTranslationKey = "locations", bTranslationDe = "Standorte", bTranslationEn = "Locations" }
    , BTranslation { bTranslationKey = "addLocation", bTranslationDe = "Standort hinzufügen", bTranslationEn = "Add location" }
    , BTranslation { bTranslationKey = "editLocation", bTranslationDe = "Standort bearbeiten", bTranslationEn = "Edit location" }
    , BTranslation { bTranslationKey = "deleteLocation", bTranslationDe = "Standort löschen", bTranslationEn = "Delete location" }
    , BTranslation { bTranslationKey = "locationMasterData", bTranslationDe = "Standort-Daten", bTranslationEn = "Location data" }

    , BTranslation { bTranslationKey = "hive", bTranslationDe = "Bienenstock", bTranslationEn = "Hive" }
    , BTranslation { bTranslationKey = "hives", bTranslationDe = "Bienenstöcke", bTranslationEn = "Hives" }
    , BTranslation { bTranslationKey = "addHive", bTranslationDe = "Bienenstock hinzufügen", bTranslationEn = "Add hive" }
    , BTranslation { bTranslationKey = "deleteHive", bTranslationDe = "Bienenstock löschen", bTranslationEn = "Delete hive" }
    , BTranslation { bTranslationKey = "editHive", bTranslationDe = "Bienenstock bearbeiten", bTranslationEn = "Edit hive" }
    , BTranslation { bTranslationKey = "hiveMasterData", bTranslationDe = "Stock-Daten", bTranslationEn = "Hive data" }

    , BTranslation { bTranslationKey = "inspection", bTranslationDe = "Durchsicht", bTranslationEn = "Inspection" }
    , BTranslation { bTranslationKey = "inspectionsAll", bTranslationDe = "Durchsichten (alle)", bTranslationEn = "Inspections (all)" }
    , BTranslation { bTranslationKey = "inspectionsLast10", bTranslationDe = "Durchsichten (letzten 10)", bTranslationEn = "Inspections (last 10)" }
    , BTranslation { bTranslationKey = "addInspection", bTranslationDe = "Durchsicht hinzufügen", bTranslationEn = "Add inspection" }
    , BTranslation { bTranslationKey = "deleteInspection", bTranslationDe = "Durchsicht löschen", bTranslationEn = "Delete inspection" }
    , BTranslation { bTranslationKey = "editInspection", bTranslationDe = "Durchsicht bearbeiten", bTranslationEn = "Edit inspection" }
    , BTranslation { bTranslationKey = "lastInspection", bTranslationDe = "Letzte Durchsicht", bTranslationEn = "Last inspection" }

    , BTranslation { bTranslationKey = "inspectionfiles", bTranslationDe = "Dateien", bTranslationEn = "Files" }
    , BTranslation { bTranslationKey = "addInspectionfile", bTranslationDe = "Durchsicht-Datei hinzufügen", bTranslationEn = "Add inspection-file" }
    , BTranslation { bTranslationKey = "deleteInspectionfile", bTranslationDe = "Durchsicht-Datei löschen", bTranslationEn = "Delete inspection-file" }
    , BTranslation { bTranslationKey = "editInspectionfile", bTranslationDe = "Durchsicht-Datei bearbeiten", bTranslationEn = "Edit inspection-file" }

    , BTranslation { bTranslationKey = "temper", bTranslationDe = "Sanftmut", bTranslationEn = "Temper" }
    , BTranslation { bTranslationKey = "temperTypes", bTranslationDe = "Sanftmut Typen", bTranslationEn = "Temper types" }
    , BTranslation { bTranslationKey = "addTemperType", bTranslationDe = "Sanftmut Typ hinzufügen", bTranslationEn = "Add temper type" }
    , BTranslation { bTranslationKey = "deleteTemperType", bTranslationDe = "Sanftmut Typ löschen", bTranslationEn = "Delete temper type" }
    , BTranslation { bTranslationKey = "editTemperType", bTranslationDe = "Sanftmut Typ bearbeiten", bTranslationEn = "Edit temper type" }

    , BTranslation { bTranslationKey = "running", bTranslationDe = "Wabensitz", bTranslationEn = "Running beh." }
    , BTranslation { bTranslationKey = "runningTypes", bTranslationDe = "Wabensitz Typen", bTranslationEn = "Running types" }
    , BTranslation { bTranslationKey = "addRunningType", bTranslationDe = "Wabensitz Typ hinzufügen", bTranslationEn = "Add running type" }
    , BTranslation { bTranslationKey = "deleteRunningType", bTranslationDe = "Wabensitz Typ löschen", bTranslationEn = "Delete running type" }
    , BTranslation { bTranslationKey = "editRunningType", bTranslationDe = "Wabensitz Typ bearbeiten", bTranslationEn = "Edit running type" }

    , BTranslation { bTranslationKey = "swarming", bTranslationDe = "Schwarmtrieb", bTranslationEn = "Swarming mood" }
    , BTranslation { bTranslationKey = "swarmingTypes", bTranslationDe = "Schwarmtrieb Typen", bTranslationEn = "Swarming types" }
    , BTranslation { bTranslationKey = "addSwarmingType", bTranslationDe = "Schwarmtrieb Typ hinzufügen", bTranslationEn = "Add swarming type" }
    , BTranslation { bTranslationKey = "deleteSwarmingType", bTranslationDe = "Schwarmtrieb Typ löschen", bTranslationEn = "Delete swarming type" }
    , BTranslation { bTranslationKey = "editSwarmingType", bTranslationDe = "Schwarmtrieb Typ bearbeiten", bTranslationEn = "Edit swarming type" }



    ]
  }
