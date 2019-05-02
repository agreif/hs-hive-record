{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson

import Generator

main :: IO ()
main = generate context

-- model context

context :: Value
context =
  toJSON $
  BContext
  { bContextCrudModels =
      [ BCrudModel
        { bCrudModelName = "user"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = ["UniqueUserIdent ident"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddUser"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditUser"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteUser"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "ident"
              , bCrudFieldLabelDe = Just "Login"
              , bCrudFieldLabelEn = Just "Login"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "password"
              , bCrudFieldLabelDe = Just "Passwort"
              , bCrudFieldLabelEn = Just "Password"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "email"
              , bCrudFieldLabelDe = Just "Email"
              , bCrudFieldLabelEn = Just "Email"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isAdmin"
              , bCrudFieldLabelDe = Just "Ist Admin?"
              , bCrudFieldLabelEn = Just "Is admin?"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "isResetPassword"
              , bCrudFieldLabelDe = Just "Neues Passwort generieren? (Wird per Email zugesendet)"
              , bCrudFieldLabelEn = Just "Generate new password? (Will be sent by email)"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Just "Nothing"
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }
      , BCrudModel
        { bCrudModelName = "config"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = ["UniqueConfigCode code"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditConfig"
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "code"
              , bCrudFieldLabelDe = Just "Code"
              , bCrudFieldLabelEn = Just "Code"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = True
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "stringValue"
              , bCrudFieldLabelDe = Just "String-Wert"
              , bCrudFieldLabelEn = Just "String-Value"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "intValue"
              , bCrudFieldLabelDe = Just "Integer-Wert"
              , bCrudFieldLabelEn = Just "Integer-Value"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "doubleValue"
              , bCrudFieldLabelDe = Just "Double-Wert"
              , bCrudFieldLabelEn = Just "Double-Value"
              , bCrudFieldHsType = "Double"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "doubleField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "boolValue"
              , bCrudFieldLabelDe = Just "Boolean-Wert"
              , bCrudFieldLabelEn = Just "Boolean-Value"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }



      , BCrudModel
        { bCrudModelName = "Rawdata"
        , bCrudModelIsJson = False
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = False
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Nothing
        , bCrudModelEditFormDataJsonUrl = Nothing
        , bCrudModelDeleteFormDataJsonUrl = Nothing
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Nothing
        , bCrudModelEditFormTitleMsg = Nothing
        , bCrudModelDeleteFormTitleMsg = Nothing
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "CrmR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "bytes"
              , bCrudFieldLabelDe = Just "Bytes"
              , bCrudFieldLabelEn = Just "Bytes"
              , bCrudFieldHsType = "ByteString"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            ]
        , bCrudModelTranslations = Nothing
        }



      , BCrudModel
        { bCrudModelName = "location"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = ["UniqueLocationName name"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "HiverecR LocationListPageDataJsonR"
        , bCrudModelEditFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataJsonR locationId"
        , bCrudModelDeleteFormDataJsonUrl = Just "HiverecR LocationListPageDataJsonR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddLocation"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditLocation"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteLocation"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "HiverecR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "name"
              , bCrudFieldLabelDe = Just "Name"
              , bCrudFieldLabelEn = Just "Name"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }



      , BCrudModel
        { bCrudModelName = "hive"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataJsonR locationId"
        , bCrudModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR hiveId"
        , bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataJsonR $ hiveLocationId hive"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = True
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddHive"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditHive"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteHive"
        , bCrudModelParentHsType = Just "Location"
        , bCrudModelFormRouteHsType = "HiverecR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "locationId"
              , bCrudFieldLabelDe = Just "Standort"
              , bCrudFieldLabelEn = Just "Location"
              , bCrudFieldHsType = "LocationId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "locationSelectField"
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "name"
              , bCrudFieldLabelDe = Just "Name"
              , bCrudFieldLabelEn = Just "Name"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "description"
              , bCrudFieldLabelDe = Just "Beschreibung"
              , bCrudFieldLabelEn = Just "Description"
              , bCrudFieldHsType = "Textarea"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textareaField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-form-width-large"
                        }
                      , BFieldAttr
                        {bFieldAttrKey = "rows", bFieldAttrValue = "5"}
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-form-width-large"
                        }
                      , BFieldAttr
                        {bFieldAttrKey = "rows", bFieldAttrValue = "5"}
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }



      , BCrudModel
        { bCrudModelName = "inspection"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = ["UniqueInspectionHiveDate hiveId date"]
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bCrudModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bCrudModelAddFormHasDefaultModel = True
        , bCrudModelEditPostLoadsModel = True
        , bCrudModelDeletePostLoadsModel = True
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddInspection"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditInspection"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteInspection"
        , bCrudModelParentHsType = Just "Hive"
        , bCrudModelFormRouteHsType = "HiverecR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "hiveId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "HiveId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "date"
              , bCrudFieldLabelDe = Just "Datum"
              , bCrudFieldLabelEn = Just "Date"
              , bCrudFieldHsType = "Day"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "dayField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "temperTypeId"
              , bCrudFieldLabelDe = Just "Sanftmut"
              , bCrudFieldLabelEn = Just "Temper"
              , bCrudFieldHsType = "TemperTypeId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "temperTypeSelectField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "runningTypeId"
              , bCrudFieldLabelDe = Just "Wabensitz"
              , bCrudFieldLabelEn = Just "Running Beh."
              , bCrudFieldHsType = "RunningTypeId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "runningTypeSelectField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "swarmingTypeId"
              , bCrudFieldLabelDe = Just "Schwarmtrieb"
              , bCrudFieldLabelEn = Just "swarming Mood"
              , bCrudFieldHsType = "SwarmingTypeId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "swarmingTypeSelectField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "queenSeen"
              , bCrudFieldLabelDe = Just "Kö ges."
              , bCrudFieldLabelEn = Just "Queen seen"
              , bCrudFieldHsType = "Bool"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "checkBoxField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue = "uk-checkbox"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "totalFrames"
              , bCrudFieldLabelDe = Just "Ges. Waben"
              , bCrudFieldLabelEn = Just "Total frames"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "beeCoveredFrames"
              , bCrudFieldLabelDe = Just "Bel. Waben"
              , bCrudFieldLabelEn = Just "Bee covered frames"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "broodFrames"
              , bCrudFieldLabelDe = Just "Brutwaben"
              , bCrudFieldLabelEn = Just "Brood frames"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "honeyFrames"
              , bCrudFieldLabelDe = Just "Honigwaben"
              , bCrudFieldLabelEn = Just "Honey frames"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "treatment"
              , bCrudFieldLabelDe = Just "Behandlung"
              , bCrudFieldLabelEn = Just "Treatment"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "feeding"
              , bCrudFieldLabelDe = Just "Fütterung"
              , bCrudFieldLabelEn = Just "Feeding"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "notes"
              , bCrudFieldLabelDe = Just "Notizen"
              , bCrudFieldLabelEn = Just "Notes"
              , bCrudFieldHsType = "Textarea"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = True
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textareaField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = False
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-textarea uk-form-small uk-width-5-6"
                        }
                      , BFieldAttr
                        { bFieldAttrKey = "rows", bFieldAttrValue = "10" }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }
      , BCrudModel
        { bCrudModelName = "inspectionfile"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bCrudModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddInspectionfile"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditInspectionfile"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteInspectionfile"
        , bCrudModelParentHsType = Just "Inspection"
        , bCrudModelFormRouteHsType = "HiverecR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "inspectionId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "InspectionId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "rawdataId"
              , bCrudFieldLabelDe = Nothing
              , bCrudFieldLabelEn = Nothing
              , bCrudFieldHsType = "RawdataId"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "filename"
              , bCrudFieldLabelDe = Just "Dateiname"
              , bCrudFieldLabelEn = Just "Filename"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "mimetype"
              , bCrudFieldLabelDe = Just "MIME Type"
              , bCrudFieldLabelEn = Just "MIME Type"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "size"
              , bCrudFieldLabelDe = Just "Groesse"
              , bCrudFieldLabelEn = Just "Size"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = False
                  }
              , bCrudFieldFormFieldType = Nothing
              , bCrudFieldAddView = Nothing
              , bCrudFieldEditView = Nothing
              }
            , BCrudField
              { bCrudFieldName = "file"
              , bCrudFieldLabelDe = Just "Datei"
              , bCrudFieldLabelEn = Just "File"
              , bCrudFieldHsType = "FileInfo"
              , bCrudFieldDb = Nothing
              , bCrudFieldFormFieldType = Just "fileField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs = []
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }












      , BCrudModel
        { bCrudModelName = "temperType"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddTemperType"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditTemperType"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteTemperType"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "name"
              , bCrudFieldLabelDe = Just "Name"
              , bCrudFieldLabelEn = Just "Name"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "sortIndex"
              , bCrudFieldLabelDe = Just "Sortierungs-Index"
              , bCrudFieldLabelEn = Just "Sort Index"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }
      , BCrudModel
        { bCrudModelName = "runningType"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddRunningType"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditRunningType"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteRunningType"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "name"
              , bCrudFieldLabelDe = Just "Name"
              , bCrudFieldLabelEn = Just "Name"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "sortIndex"
              , bCrudFieldLabelDe = Just "Sortierungs-Index"
              , bCrudFieldLabelEn = Just "Sort Index"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }
      , BCrudModel
        { bCrudModelName = "swarmingType"
        , bCrudModelIsJson = True
        , bCrudModelDbUniquenesses = []
        , bCrudModelDbHasHistoryTable = True
        , bCrudModelHsDerivings = []
        , bCrudModelAddFormArgs = Nothing
        , bCrudModelEditFormArgs = Nothing
        , bCrudModelAddFormEntityLoader = Nothing
        , bCrudModelEditFormEntityLoader = Nothing
        , bCrudModelDeleteFormEntityLoader = Nothing
        , bCrudModelAddFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelEditFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataJsonR"
        , bCrudModelAddFormHasDefaultModel = False
        , bCrudModelEditPostLoadsModel = False
        , bCrudModelDeletePostLoadsModel = False
        , bCrudModelAddPostExtraStoreFunc = Nothing
        , bCrudModelEditPostExtraStoreFunc = Nothing
        , bCrudModelAddFormTitleMsg = Just "MsgGlobalAddSwarmingType"
        , bCrudModelEditFormTitleMsg = Just "MsgGlobalEditSwarmingType"
        , bCrudModelDeleteFormTitleMsg = Just "MsgGlobalDeleteSwarmingType"
        , bCrudModelParentHsType = Nothing
        , bCrudModelFormRouteHsType = "AdminR"
        , bCrudModelFields =
            [ BCrudField
              { bCrudFieldName = "name"
              , bCrudFieldLabelDe = Just "Name"
              , bCrudFieldLabelEn = Just "Name"
              , bCrudFieldHsType = "Text"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "textField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            , BCrudField
              { bCrudFieldName = "sortIndex"
              , bCrudFieldLabelDe = Just "Sortierungs-Index"
              , bCrudFieldLabelEn = Just "Sort Index"
              , bCrudFieldHsType = "Int"
              , bCrudFieldDb =
                  Just $
                  BCrudFieldDb
                  { bCrudFieldDbIsNullable = False
                  , bCrudFieldDbDefault = Nothing
                  , bCrudFieldDbCanUpdate = True
                  }
              , bCrudFieldFormFieldType = Just "intField"
              , bCrudFieldAddView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              , bCrudFieldEditView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-medium"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bCrudModelTranslations = Nothing
        }













      ]


  , bContextActionModels =
      [ BActionModel
        { bActionModelName = "testmail"
        , bActionModelAction = "send"
        , bActionModelFormArgs = Nothing
        , bActionModelFormEntityLoader = Nothing
        , bActionModelFormDataJsonUrl = Nothing
        , bActionModelFormHasDefaultModel = False
        , bActionModelPostExtraStoreFunc = Nothing
        , bActionModelFormTitleMsg = Just "MsgTestmailSendTestMail"
        , bActionModelFormRouteHsType = "AdminR"
        , bActionModelFields =
            [ BActionField
              { bActionFieldName = "email"
              , bActionFieldLabelDe = Just "Email"
              , bActionFieldLabelEn = Just "Email"
              , bActionFieldHsType = "Text"
              , bActionFieldFormFieldType = Just "textField"
              , bActionFieldView =
                  Just $
                  BFieldView
                  { bFieldViewIsRequired = True
                  , bFieldViewIsDisabled = False
                  , bFieldViewAttrs =
                      [ BFieldAttr
                        { bFieldAttrKey = "class"
                        , bFieldAttrValue =
                            "uk-input uk-form-small uk-form-width-large"
                        }
                      ]
                  , bFieldViewDefault = Nothing
                  }
              }
            ]
        , bActionModelTranslations =
          Just
          [ BTranslation { bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail" }
          , BTranslation { bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..." }
          ]
        }

    ]


  , bContextGlobalTranslations =
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
    , BTranslation { bTranslationKey = "detailHive", bTranslationDe = "Bienenstock Details", bTranslationEn = "Hive details" }
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
