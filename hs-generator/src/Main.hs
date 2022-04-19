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
              { bCrudModelName = "user",
                bCrudModelIsJson = False,
                bCrudModelDbUniquenesses = ["UniqueUserIdent ident"],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Nothing,
                bCrudModelEditFormDataJsonUrl = Nothing,
                bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataR",
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgUserAddUser",
                bCrudModelEditFormTitleMsg = Just "MsgUserEditUser",
                bCrudModelDeleteFormTitleMsg = Just "MsgUserDeleteUser",
                bCrudModelParentHsType = Nothing,
                bCrudModelFormRouteHsType = "AdminR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "ident",
                        bCrudFieldLabelDe = Just "Login",
                        bCrudFieldLabelEn = Just "Login",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "password",
                        bCrudFieldLabelDe = Just "Passwort",
                        bCrudFieldLabelEn = Just "Password",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "email",
                        bCrudFieldLabelDe = Just "Email",
                        bCrudFieldLabelEn = Just "Email",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "isAdmin",
                        bCrudFieldLabelDe = Just "Ist Admin?",
                        bCrudFieldLabelEn = Just "Is admin?",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Bool",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "checkBoxField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "isResetPassword",
                        bCrudFieldLabelDe = Just "Neues Passwort generieren? (Wird per Email zugesendet)",
                        bCrudFieldLabelEn = Just "Generate new password? (Will be sent by email)",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Bool",
                        bCrudFieldDb = Nothing,
                        bCrudFieldFormFieldType = Just "checkBoxField",
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Just "Nothing"
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "users", bTranslationDe = "Nutzer", bTranslationEn = "Users"},
                      BTranslation {bTranslationKey = "addUser", bTranslationDe = "Nutzer hinzufügen", bTranslationEn = "Add user"},
                      BTranslation {bTranslationKey = "editUser", bTranslationDe = "Nutzer bearbeiten", bTranslationEn = "Edit user"},
                      BTranslation {bTranslationKey = "deleteUser", bTranslationDe = "Nutzer löschen", bTranslationEn = "Delete user"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "config",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = ["UniqueConfigCode code"],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Nothing,
                bCrudModelEditFormDataJsonUrl = Just "AdminR AdminPageDataR",
                bCrudModelDeleteFormDataJsonUrl = Nothing,
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Nothing,
                bCrudModelEditFormTitleMsg = Just "MsgGlobalEditConfig",
                bCrudModelDeleteFormTitleMsg = Nothing,
                bCrudModelParentHsType = Nothing,
                bCrudModelFormRouteHsType = "AdminR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "code",
                        bCrudFieldLabelDe = Just "Code",
                        bCrudFieldLabelEn = Just "Code",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = True,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "stringValue",
                        bCrudFieldLabelDe = Just "String-Wert",
                        bCrudFieldLabelEn = Just "String-Value",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "intValue",
                        bCrudFieldLabelDe = Just "Integer-Wert",
                        bCrudFieldLabelEn = Just "Integer-Value",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "intField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "doubleValue",
                        bCrudFieldLabelDe = Just "Double-Wert",
                        bCrudFieldLabelEn = Just "Double-Value",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Double",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "doubleField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "boolValue",
                        bCrudFieldLabelDe = Just "Boolean-Wert",
                        bCrudFieldLabelEn = Just "Boolean-Value",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Bool",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "checkBoxField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations = Nothing
              },
            BCrudModel
              { bCrudModelName = "rawdata",
                bCrudModelIsJson = False,
                bCrudModelDbUniquenesses = [],
                bCrudModelDbHasHistoryTable = False,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Nothing,
                bCrudModelEditFormDataJsonUrl = Nothing,
                bCrudModelDeleteFormDataJsonUrl = Nothing,
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Nothing,
                bCrudModelEditFormTitleMsg = Nothing,
                bCrudModelDeleteFormTitleMsg = Nothing,
                bCrudModelParentHsType = Nothing,
                bCrudModelFormRouteHsType = "CrmR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "bytes",
                        bCrudFieldLabelDe = Just "Bytes",
                        bCrudFieldLabelEn = Just "Bytes",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "ByteString",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      }
                  ],
                bCrudModelTranslations = Nothing
              },
            BCrudModel
              { bCrudModelName = "location",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = ["UniqueLocationName name"],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "HiverecR LocationListPageDataR",
                bCrudModelEditFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataR locationId",
                bCrudModelDeleteFormDataJsonUrl = Just "HiverecR LocationListPageDataR",
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgLocationAddLocation",
                bCrudModelEditFormTitleMsg = Just "MsgLocationEditLocation",
                bCrudModelDeleteFormTitleMsg = Just "MsgLocationDeleteLocation",
                bCrudModelParentHsType = Nothing,
                bCrudModelFormRouteHsType = "HiverecR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "name",
                        bCrudFieldLabelDe = Just "Name",
                        bCrudFieldLabelEn = Just "Name",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "location", bTranslationDe = "Standort", bTranslationEn = "Location"},
                      BTranslation {bTranslationKey = "locations", bTranslationDe = "Standorte", bTranslationEn = "Locations"},
                      BTranslation {bTranslationKey = "addLocation", bTranslationDe = "Standort hinzufügen", bTranslationEn = "Add location"},
                      BTranslation {bTranslationKey = "editLocation", bTranslationDe = "Standort bearbeiten", bTranslationEn = "Edit location"},
                      BTranslation {bTranslationKey = "deleteLocation", bTranslationDe = "Standort löschen", bTranslationEn = "Delete location"},
                      BTranslation {bTranslationKey = "locationMasterData", bTranslationDe = "Standort-Daten", bTranslationEn = "Location data"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "hive",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = [],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataR locationId",
                bCrudModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR hiveId",
                bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ LocationDetailPageDataR $ hiveLocationId hive",
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = True,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgHiveAddHive",
                bCrudModelEditFormTitleMsg = Just "MsgHiveEditHive",
                bCrudModelDeleteFormTitleMsg = Just "MsgHiveDeleteHive",
                bCrudModelParentHsType = Just "Location",
                bCrudModelFormRouteHsType = "HiverecR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "locationId",
                        bCrudFieldLabelDe = Just "Standort",
                        bCrudFieldLabelEn = Just "Location",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "LocationId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "locationSelectField",
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "name",
                        bCrudFieldLabelDe = Just "Name",
                        bCrudFieldLabelEn = Just "Name",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "queenYear",
                        bCrudFieldLabelDe = Just "Königin Jahr",
                        bCrudFieldLabelEn = Just "Queen Year",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "intField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "description",
                        bCrudFieldLabelDe = Just "Beschreibung",
                        bCrudFieldLabelEn = Just "Description",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Textarea",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textareaField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-textarea uk-form-small uk-form-width-large"
                                      },
                                    BFieldAttr
                                      { bFieldAttrKey = "rows",
                                        bFieldAttrValue = "5"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-textarea uk-form-small uk-form-width-large"
                                      },
                                    BFieldAttr
                                      { bFieldAttrKey = "rows",
                                        bFieldAttrValue = "5"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "isDissolved",
                        bCrudFieldLabelDe = Just "Ist aufgelöst?",
                        bCrudFieldLabelEn = Just "Is dissolved?",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Bool",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "checkBoxField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "hiveOverview", bTranslationDe = "Bienenstock Übersicht", bTranslationEn = "Hive Overview"},
                      BTranslation {bTranslationKey = "hive", bTranslationDe = "Bienenstock", bTranslationEn = "Hive"},
                      BTranslation {bTranslationKey = "hives", bTranslationDe = "Bienenstöcke", bTranslationEn = "Hives"},
                      BTranslation {bTranslationKey = "addHive", bTranslationDe = "Bienenstock hinzufügen", bTranslationEn = "Add hive"},
                      BTranslation {bTranslationKey = "deleteHive", bTranslationDe = "Bienenstock löschen", bTranslationEn = "Delete hive"},
                      BTranslation {bTranslationKey = "detailHive", bTranslationDe = "Bienenstock Details", bTranslationEn = "Hive details"},
                      BTranslation {bTranslationKey = "editHive", bTranslationDe = "Bienenstock bearbeiten", bTranslationEn = "Edit hive"},
                      BTranslation {bTranslationKey = "hiveMasterData", bTranslationDe = "Stock-Daten", bTranslationEn = "Hive data"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "inspection",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = ["UniqueInspectionHiveDate hiveId date"],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "getAddInspectionSuccessDataJsonUrl inspection maybeCurRoute", -- "HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection"
                bCrudModelEditFormDataJsonUrl = Just "getEditInspectionSuccessDataJsonUrl (inspectionHiveId inspection) maybeCurRoute", -- "HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection"
                bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection",
                bCrudModelAddFormHasDefaultModel = True,
                bCrudModelEditPostLoadsModel = True,
                bCrudModelDeletePostLoadsModel = True,
                bCrudModelAddPostExtraStoreFunc = Just "storeInspectionDateToSession vAddInspection inspectionId",
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = True,
                bCrudModelEditPostNeedsCurRoute = True,
                bCrudModelAddFormTitleMsg = Just "MsgInspectionAddInspection",
                bCrudModelEditFormTitleMsg = Just "MsgInspectionEditInspection",
                bCrudModelDeleteFormTitleMsg = Just "MsgInspectionDeleteInspection",
                bCrudModelParentHsType = Just "Hive",
                bCrudModelFormRouteHsType = "HiverecR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "hiveId",
                        bCrudFieldLabelDe = Nothing,
                        bCrudFieldLabelEn = Nothing,
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "HiveId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "date",
                        bCrudFieldLabelDe = Just "Datum",
                        bCrudFieldLabelEn = Just "Date",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Day",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "dayField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "swarmingTypeId",
                        bCrudFieldLabelDe = Just "Schwarmtrieb",
                        bCrudFieldLabelEn = Just "swarming Mood",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "SwarmingTypeId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "swarmingTypeSelectField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "queenSeen",
                        bCrudFieldLabelDe = Just "Kö ges.",
                        bCrudFieldLabelEn = Just "Queen seen",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Bool",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "checkBoxField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue = "uk-checkbox"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "beeCoveredFrames",
                        bCrudFieldLabelDe = Just "Bel. Waben",
                        bCrudFieldLabelEn = Just "Bee covered frames",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "intField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "broodFrames",
                        bCrudFieldLabelDe = Just "Brutwaben",
                        bCrudFieldLabelEn = Just "Brood frames",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "intField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "honeyFrames",
                        bCrudFieldLabelDe = Just "Honigwaben",
                        bCrudFieldLabelEn = Just "Honey frames",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "intField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "treatment",
                        bCrudFieldLabelDe = Just "Behandlung",
                        bCrudFieldLabelEn = Just "Treatment",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "feeding",
                        bCrudFieldLabelDe = Just "Fütterung",
                        bCrudFieldLabelEn = Just "Feeding",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "notes",
                        bCrudFieldLabelDe = Just "Notizen",
                        bCrudFieldLabelEn = Just "Notes",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Textarea",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = True,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textareaField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-textarea uk-form-small uk-width-5-6"
                                      },
                                    BFieldAttr
                                      { bFieldAttrKey = "rows",
                                        bFieldAttrValue = "10"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = False,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-textarea uk-form-small uk-width-5-6"
                                      },
                                    BFieldAttr
                                      { bFieldAttrKey = "rows",
                                        bFieldAttrValue = "10"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "inspection", bTranslationDe = "Durchsicht", bTranslationEn = "Inspection"},
                      BTranslation {bTranslationKey = "inspections", bTranslationDe = "Durchsichten", bTranslationEn = "Inspections"},
                      BTranslation {bTranslationKey = "addInspection", bTranslationDe = "Durchsicht hinzufügen", bTranslationEn = "Add inspection"},
                      BTranslation {bTranslationKey = "deleteInspection", bTranslationDe = "Durchsicht löschen", bTranslationEn = "Delete inspection"},
                      BTranslation {bTranslationKey = "editInspection", bTranslationDe = "Durchsicht bearbeiten", bTranslationEn = "Edit inspection"},
                      BTranslation {bTranslationKey = "lastInspection", bTranslationDe = "Letzte Durchsicht", bTranslationEn = "Last inspection"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "inspectionfile",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = [],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection",
                bCrudModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection",
                bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection",
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgInspectionfileAddInspectionfile",
                bCrudModelEditFormTitleMsg = Just "MsgInspectionfileEditInspectionfile",
                bCrudModelDeleteFormTitleMsg = Just "MsgInspectionfileDeleteInspectionfile",
                bCrudModelParentHsType = Just "Inspection",
                bCrudModelFormRouteHsType = "HiverecR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "inspectionId",
                        bCrudFieldLabelDe = Nothing,
                        bCrudFieldLabelEn = Nothing,
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "InspectionId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "rawdataId",
                        bCrudFieldLabelDe = Nothing,
                        bCrudFieldLabelEn = Nothing,
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "RawdataId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "filename",
                        bCrudFieldLabelDe = Just "Dateiname",
                        bCrudFieldLabelEn = Just "Filename",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "mimetype",
                        bCrudFieldLabelDe = Just "MIME Type",
                        bCrudFieldLabelEn = Just "MIME Type",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "size",
                        bCrudFieldLabelDe = Just "Groesse",
                        bCrudFieldLabelEn = Just "Size",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "file",
                        bCrudFieldLabelDe = Just "Datei",
                        bCrudFieldLabelEn = Just "File",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "FileInfo",
                        bCrudFieldDb = Nothing,
                        bCrudFieldFormFieldType = Just "fileField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "inspectionfiles", bTranslationDe = "Dateien", bTranslationEn = "Files"},
                      BTranslation {bTranslationKey = "addInspectionfile", bTranslationDe = "Durchsicht-Datei hinzufügen", bTranslationEn = "Add inspection-file"},
                      BTranslation {bTranslationKey = "deleteInspectionfile", bTranslationDe = "Durchsicht-Datei löschen", bTranslationEn = "Delete inspection-file"},
                      BTranslation {bTranslationKey = "editInspectionfile", bTranslationDe = "Durchsicht-Datei bearbeiten", bTranslationEn = "Edit inspection-file"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "swarmingType",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = [],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "AdminR AdminPageDataR",
                bCrudModelEditFormDataJsonUrl = Just "AdminR AdminPageDataR",
                bCrudModelDeleteFormDataJsonUrl = Just "AdminR AdminPageDataR",
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgSwarmingTypeAddSwarmingType",
                bCrudModelEditFormTitleMsg = Just "MsgSwarmingTypeEditSwarmingType",
                bCrudModelDeleteFormTitleMsg = Just "MsgSwarmingTypeDeleteSwarmingType",
                bCrudModelParentHsType = Nothing,
                bCrudModelFormRouteHsType = "AdminR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "name",
                        bCrudFieldLabelDe = Just "Name",
                        bCrudFieldLabelEn = Just "Name",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "sortIndex",
                        bCrudFieldLabelDe = Just "Sortierungs-Index",
                        bCrudFieldLabelEn = Just "Sort Index",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "intField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-medium"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "swarming", bTranslationDe = "Schwarmtrieb", bTranslationEn = "Swarming mood"},
                      BTranslation {bTranslationKey = "swarmingTypes", bTranslationDe = "Schwarmtrieb Typen", bTranslationEn = "Swarming types"},
                      BTranslation {bTranslationKey = "addSwarmingType", bTranslationDe = "Schwarmtrieb Typ hinzufügen", bTranslationEn = "Add swarming type"},
                      BTranslation {bTranslationKey = "deleteSwarmingType", bTranslationDe = "Schwarmtrieb Typ löschen", bTranslationEn = "Delete swarming type"},
                      BTranslation {bTranslationKey = "editSwarmingType", bTranslationDe = "Schwarmtrieb Typ bearbeiten", bTranslationEn = "Edit swarming type"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "note",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = ["UniqueNoteDate date"],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "HiverecR NoteListDataR",
                bCrudModelEditFormDataJsonUrl = Just "HiverecR NoteListDataR",
                bCrudModelDeleteFormDataJsonUrl = Just "HiverecR NoteListDataR",
                bCrudModelAddFormHasDefaultModel = True,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgNoteAddNote",
                bCrudModelEditFormTitleMsg = Just "MsgNoteEditNote",
                bCrudModelDeleteFormTitleMsg = Just "MsgNoteDeleteNote",
                bCrudModelParentHsType = Nothing,
                bCrudModelFormRouteHsType = "HiverecR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "date",
                        bCrudFieldLabelDe = Just "Datum",
                        bCrudFieldLabelEn = Just "Date",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Day",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "dayField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              }
                      },
                    BCrudField
                      { bCrudFieldName = "text",
                        bCrudFieldLabelDe = Just "Text",
                        bCrudFieldLabelEn = Just "Text",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Textarea",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = True
                              },
                        bCrudFieldFormFieldType = Just "textareaField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-textarea uk-form-small uk-width-5-6"
                                      },
                                    BFieldAttr
                                      { bFieldAttrKey = "rows",
                                        bFieldAttrValue = "10"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-textarea uk-form-small uk-width-5-6"
                                      },
                                    BFieldAttr
                                      { bFieldAttrKey = "rows",
                                        bFieldAttrValue = "10"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "note", bTranslationDe = "Notiz", bTranslationEn = "Note"},
                      BTranslation {bTranslationKey = "notes", bTranslationDe = "Notizen", bTranslationEn = "Notes"},
                      BTranslation {bTranslationKey = "addNote", bTranslationDe = "Notiz hinzufügen", bTranslationEn = "Add note"},
                      BTranslation {bTranslationKey = "editNote", bTranslationDe = "Notiz bearbeiten", bTranslationEn = "Edit note"},
                      BTranslation {bTranslationKey = "deleteNote", bTranslationDe = "Notiz löschen", bTranslationEn = "Delete note"}
                    ]
              },
            BCrudModel
              { bCrudModelName = "notefile",
                bCrudModelIsJson = True,
                bCrudModelDbUniquenesses = [],
                bCrudModelDbHasHistoryTable = True,
                bCrudModelHsDerivings = [],
                bCrudModelAddFormArgs = Nothing,
                bCrudModelEditFormArgs = Nothing,
                bCrudModelAddHandlerArgs = Nothing,
                bCrudModelEditHandlerArgs = Nothing,
                bCrudModelAddFormEntityLoader = Nothing,
                bCrudModelEditFormEntityLoader = Nothing,
                bCrudModelDeleteFormEntityLoader = Nothing,
                bCrudModelAddFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ noteHiveId note",
                bCrudModelEditFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ noteHiveId note",
                bCrudModelDeleteFormDataJsonUrl = Just "HiverecR $ HiveDetailPageDataR $ noteHiveId note",
                bCrudModelAddFormHasDefaultModel = False,
                bCrudModelEditPostLoadsModel = False,
                bCrudModelDeletePostLoadsModel = False,
                bCrudModelAddPostExtraStoreFunc = Nothing,
                bCrudModelEditPostExtraStoreFunc = Nothing,
                bCrudModelAddPostNeedsCurRoute = False,
                bCrudModelEditPostNeedsCurRoute = False,
                bCrudModelAddFormTitleMsg = Just "MsgNotefileAddNotefile",
                bCrudModelEditFormTitleMsg = Just "MsgNotefileEditNotefile",
                bCrudModelDeleteFormTitleMsg = Just "MsgNotefileDeleteNotefile",
                bCrudModelParentHsType = Just "Note",
                bCrudModelFormRouteHsType = "HiverecR",
                bCrudModelFields =
                  [ BCrudField
                      { bCrudFieldName = "noteId",
                        bCrudFieldLabelDe = Nothing,
                        bCrudFieldLabelEn = Nothing,
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "NoteId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "rawdataId",
                        bCrudFieldLabelDe = Nothing,
                        bCrudFieldLabelEn = Nothing,
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "RawdataId",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "filename",
                        bCrudFieldLabelDe = Just "Dateiname",
                        bCrudFieldLabelEn = Just "Filename",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "mimetype",
                        bCrudFieldLabelDe = Just "MIME Type",
                        bCrudFieldLabelEn = Just "MIME Type",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Text",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "size",
                        bCrudFieldLabelDe = Just "Groesse",
                        bCrudFieldLabelEn = Just "Size",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "Int",
                        bCrudFieldDb =
                          Just $
                            BCrudFieldDb
                              { bCrudFieldDbIsNullable = False,
                                bCrudFieldDbDefault = Nothing,
                                bCrudFieldDbCanUpdate = False
                              },
                        bCrudFieldFormFieldType = Nothing,
                        bCrudFieldAddView = Nothing,
                        bCrudFieldEditView = Nothing
                      },
                    BCrudField
                      { bCrudFieldName = "file",
                        bCrudFieldLabelDe = Just "Datei",
                        bCrudFieldLabelEn = Just "File",
                        bCrudFieldInfoDe = Nothing,
                        bCrudFieldInfoEn = Nothing,
                        bCrudFieldHsType = "FileInfo",
                        bCrudFieldDb = Nothing,
                        bCrudFieldFormFieldType = Just "fileField",
                        bCrudFieldAddView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              },
                        bCrudFieldEditView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs = [],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bCrudModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "notefiles", bTranslationDe = "Dateien", bTranslationEn = "Files"},
                      BTranslation {bTranslationKey = "addNotefile", bTranslationDe = "Notiz-Datei hinzufügen", bTranslationEn = "Add note-file"},
                      BTranslation {bTranslationKey = "deleteNotefile", bTranslationDe = "Notiz-Datei löschen", bTranslationEn = "Delete note-file"},
                      BTranslation {bTranslationKey = "editNotefile", bTranslationDe = "Notiz-Datei bearbeiten", bTranslationEn = "Edit note-file"}
                    ]
              }
          ],
        bContextActionModels =
          [ BActionModel
              { bActionModelName = "testmail",
                bActionModelAction = "send",
                bActionModelHandlerArgs = Nothing,
                bActionModelFormArgs = Nothing,
                bActionModelFormEntityLoader = Nothing,
                bActionModelFormDataJsonUrl = Nothing,
                bActionModelFormHasDefaultModel = False,
                bActionModelFormTitleMsg = Just "MsgTestmailSendTestMail",
                bActionModelParentHsType = Nothing,
                bActionModelFormRouteHsType = "AdminR",
                bActionModelFields =
                  [ BActionField
                      { bActionFieldName = "email",
                        bActionFieldLabelDe = Just "Email",
                        bActionFieldLabelEn = Just "Email",
                        bActionFieldInfoDe = Nothing,
                        bActionFieldInfoEn = Nothing,
                        bActionFieldHsType = "Text",
                        bActionFieldFormFieldType = Just "textField",
                        bActionFieldView =
                          Just $
                            BFieldView
                              { bFieldViewIsRequired = True,
                                bFieldViewIsDisabled = False,
                                bFieldViewAttrs =
                                  [ BFieldAttr
                                      { bFieldAttrKey = "class",
                                        bFieldAttrValue =
                                          "uk-input uk-form-small uk-form-width-large"
                                      }
                                  ],
                                bFieldViewDefault = Nothing
                              }
                      }
                  ],
                bActionModelTranslations =
                  Just
                    [ BTranslation {bTranslationKey = "testMail", bTranslationDe = "Test-Mail", bTranslationEn = "Test-Mail"},
                      BTranslation {bTranslationKey = "sendTestMail", bTranslationDe = "Test-Mail senden...", bTranslationEn = "Send Test-Mail..."}
                    ]
              }
          ],
        bContextGlobalTranslations =
          [ BTranslation {bTranslationKey = "home", bTranslationDe = "Home", bTranslationEn = "Home"},
            BTranslation {bTranslationKey = "admin", bTranslationDe = "Admin", bTranslationEn = "Admin"},
            BTranslation {bTranslationKey = "logout", bTranslationDe = "Logout", bTranslationEn = "Logout"},
            BTranslation {bTranslationKey = "language", bTranslationDe = "Sprache", bTranslationEn = "Language"},
            BTranslation {bTranslationKey = "myProfile", bTranslationDe = "Mein Profil", bTranslationEn = "My Profile"},
            BTranslation {bTranslationKey = "editMyProfile", bTranslationDe = "Mein Profil bearbeiten", bTranslationEn = "Edit my profile"},
            BTranslation {bTranslationKey = "reallyDelete", bTranslationDe = "Möchten sie wirklich löschen?", bTranslationEn = "Are you sure to delete?"},
            BTranslation {bTranslationKey = "showMore", bTranslationDe = "Mehr anzeigen", bTranslationEn = "Show More"},
            BTranslation {bTranslationKey = "showLess", bTranslationDe = "Weniger anzeigen", bTranslationEn = "Show Less"},
            BTranslation {bTranslationKey = "showAll", bTranslationDe = "Alle anzeigen", bTranslationEn = "Show All"},
            BTranslation {bTranslationKey = "configurations", bTranslationDe = "Konfigurationen", bTranslationEn = "Configurations"},
            BTranslation {bTranslationKey = "editConfig", bTranslationDe = "Konfiguration bearbeiten", bTranslationEn = "Edit config"},
            BTranslation {bTranslationKey = "cancel", bTranslationDe = "Abbrechen", bTranslationEn = "Cancel"},
            BTranslation {bTranslationKey = "info", bTranslationDe = "Info", bTranslationEn = "Info"}
          ]
      }
