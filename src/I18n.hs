{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module I18n where

import ClassyPrelude.Yesod

-- gen i18n - start
data AppMessage =
  MsgGlobalHome
  | MsgGlobalAdmin
  | MsgGlobalLogout
  | MsgGlobalLanguage
  | MsgGlobalMyProfile
  | MsgGlobalEditMyProfile
  | MsgGlobalReallyDelete
  | MsgGlobalUsers
  | MsgGlobalAddUser
  | MsgGlobalEditUser
  | MsgGlobalDeleteUser
  | MsgGlobalConfigurations
  | MsgGlobalEditConfig
  | MsgGlobalTestMail
  | MsgGlobalSendTestMail
  | MsgGlobalCancel
  | MsgGlobalLocation
  | MsgGlobalLocations
  | MsgGlobalAddLocation
  | MsgGlobalEditLocation
  | MsgGlobalDeleteLocation
  | MsgGlobalLocationMasterData
  | MsgGlobalHive
  | MsgGlobalHives
  | MsgGlobalAddHive
  | MsgGlobalDeleteHive
  | MsgGlobalDetailHive
  | MsgGlobalEditHive
  | MsgGlobalHiveMasterData
  | MsgGlobalInspection
  | MsgGlobalInspectionsAll
  | MsgGlobalInspectionsLast10
  | MsgGlobalAddInspection
  | MsgGlobalDeleteInspection
  | MsgGlobalEditInspection
  | MsgGlobalLastInspection
  | MsgGlobalInspectionfiles
  | MsgGlobalAddInspectionfile
  | MsgGlobalDeleteInspectionfile
  | MsgGlobalEditInspectionfile
  | MsgGlobalTemper
  | MsgGlobalTemperTypes
  | MsgGlobalAddTemperType
  | MsgGlobalDeleteTemperType
  | MsgGlobalEditTemperType
  | MsgGlobalRunning
  | MsgGlobalRunningTypes
  | MsgGlobalAddRunningType
  | MsgGlobalDeleteRunningType
  | MsgGlobalEditRunningType
  | MsgGlobalSwarming
  | MsgGlobalSwarmingTypes
  | MsgGlobalAddSwarmingType
  | MsgGlobalDeleteSwarmingType
  | MsgGlobalEditSwarmingType
  | MsgUserIdent
  | MsgUserPassword
  | MsgUserEmail
  | MsgUserIsAdmin
  | MsgUserIsResetPassword
  | MsgConfigCode
  | MsgConfigStringValue
  | MsgConfigIntValue
  | MsgConfigDoubleValue
  | MsgConfigBoolValue
  | MsgTestmailEmail
  | MsgRawdataBytes
  | MsgLocationName
  | MsgHiveLocationId
  | MsgHiveName
  | MsgHiveDescription
  | MsgInspectionHiveId
  | MsgInspectionDate
  | MsgInspectionTemperTypeId
  | MsgInspectionRunningTypeId
  | MsgInspectionSwarmingTypeId
  | MsgInspectionQueenSeen
  | MsgInspectionTotalFrames
  | MsgInspectionBeeCoveredFrames
  | MsgInspectionBroodFrames
  | MsgInspectionHoneyFrames
  | MsgInspectionTreatment
  | MsgInspectionFeeding
  | MsgInspectionNotes
  | MsgInspectionfileInspectionId
  | MsgInspectionfileRawdataId
  | MsgInspectionfileFilename
  | MsgInspectionfileMimetype
  | MsgInspectionfileSize
  | MsgInspectionfileFile
  | MsgTemperTypeName
  | MsgTemperTypeSortIndex
  | MsgRunningTypeName
  | MsgRunningTypeSortIndex
  | MsgSwarmingTypeName
  | MsgSwarmingTypeSortIndex

renderMessageGerman :: AppMessage -> Text
renderMessageGerman MsgGlobalHome = "Home"
renderMessageGerman MsgGlobalAdmin = "Admin"
renderMessageGerman MsgGlobalLogout = "Logout"
renderMessageGerman MsgGlobalLanguage = "Sprache"
renderMessageGerman MsgGlobalMyProfile = "Mein Profil"
renderMessageGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderMessageGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderMessageGerman MsgGlobalUsers = "Nutzer"
renderMessageGerman MsgGlobalAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgGlobalEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgGlobalDeleteUser = "Nutzer löschen"
renderMessageGerman MsgGlobalConfigurations = "Konfigurationen"
renderMessageGerman MsgGlobalEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgGlobalTestMail = "Test-Mail"
renderMessageGerman MsgGlobalSendTestMail = "Test-Mail senden..."
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgGlobalLocation = "Standort"
renderMessageGerman MsgGlobalLocations = "Standorte"
renderMessageGerman MsgGlobalAddLocation = "Standort hinzufügen"
renderMessageGerman MsgGlobalEditLocation = "Standort bearbeiten"
renderMessageGerman MsgGlobalDeleteLocation = "Standort löschen"
renderMessageGerman MsgGlobalLocationMasterData = "Standort-Daten"
renderMessageGerman MsgGlobalHive = "Bienenstock"
renderMessageGerman MsgGlobalHives = "Bienenstöcke"
renderMessageGerman MsgGlobalAddHive = "Bienenstock hinzufügen"
renderMessageGerman MsgGlobalDeleteHive = "Bienenstock löschen"
renderMessageGerman MsgGlobalDetailHive = "Bienenstock Details"
renderMessageGerman MsgGlobalEditHive = "Bienenstock bearbeiten"
renderMessageGerman MsgGlobalHiveMasterData = "Stock-Daten"
renderMessageGerman MsgGlobalInspection = "Durchsicht"
renderMessageGerman MsgGlobalInspectionsAll = "Durchsichten (alle)"
renderMessageGerman MsgGlobalInspectionsLast10 = "Durchsichten (letzten 10)"
renderMessageGerman MsgGlobalAddInspection = "Durchsicht hinzufügen"
renderMessageGerman MsgGlobalDeleteInspection = "Durchsicht löschen"
renderMessageGerman MsgGlobalEditInspection = "Durchsicht bearbeiten"
renderMessageGerman MsgGlobalLastInspection = "Letzte Durchsicht"
renderMessageGerman MsgGlobalInspectionfiles = "Dateien"
renderMessageGerman MsgGlobalAddInspectionfile = "Durchsicht-Datei hinzufügen"
renderMessageGerman MsgGlobalDeleteInspectionfile = "Durchsicht-Datei löschen"
renderMessageGerman MsgGlobalEditInspectionfile = "Durchsicht-Datei bearbeiten"
renderMessageGerman MsgGlobalTemper = "Sanftmut"
renderMessageGerman MsgGlobalTemperTypes = "Sanftmut Typen"
renderMessageGerman MsgGlobalAddTemperType = "Sanftmut Typ hinzufügen"
renderMessageGerman MsgGlobalDeleteTemperType = "Sanftmut Typ löschen"
renderMessageGerman MsgGlobalEditTemperType = "Sanftmut Typ bearbeiten"
renderMessageGerman MsgGlobalRunning = "Wabensitz"
renderMessageGerman MsgGlobalRunningTypes = "Wabensitz Typen"
renderMessageGerman MsgGlobalAddRunningType = "Wabensitz Typ hinzufügen"
renderMessageGerman MsgGlobalDeleteRunningType = "Wabensitz Typ löschen"
renderMessageGerman MsgGlobalEditRunningType = "Wabensitz Typ bearbeiten"
renderMessageGerman MsgGlobalSwarming = "Schwarmtrieb"
renderMessageGerman MsgGlobalSwarmingTypes = "Schwarmtrieb Typen"
renderMessageGerman MsgGlobalAddSwarmingType = "Schwarmtrieb Typ hinzufügen"
renderMessageGerman MsgGlobalDeleteSwarmingType = "Schwarmtrieb Typ löschen"
renderMessageGerman MsgGlobalEditSwarmingType = "Schwarmtrieb Typ bearbeiten"
renderMessageGerman MsgUserIdent = "Login"
renderMessageGerman MsgUserPassword = "Passwort"
renderMessageGerman MsgUserEmail = "Email"
renderMessageGerman MsgUserIsAdmin = "Ist Admin?"
renderMessageGerman MsgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
renderMessageGerman MsgConfigCode = "Code"
renderMessageGerman MsgConfigStringValue = "String-Wert"
renderMessageGerman MsgConfigIntValue = "Integer-Wert"
renderMessageGerman MsgConfigDoubleValue = "Double-Wert"
renderMessageGerman MsgConfigBoolValue = "Boolean-Wert"
renderMessageGerman MsgTestmailEmail = "Email"
renderMessageGerman MsgRawdataBytes = "Bytes"
renderMessageGerman MsgLocationName = "Name"
renderMessageGerman MsgHiveLocationId = ""
renderMessageGerman MsgHiveName = "Name"
renderMessageGerman MsgHiveDescription = "Beschreibung"
renderMessageGerman MsgInspectionHiveId = ""
renderMessageGerman MsgInspectionDate = "Datum"
renderMessageGerman MsgInspectionTemperTypeId = "Sanftmut"
renderMessageGerman MsgInspectionRunningTypeId = "Wabensitz"
renderMessageGerman MsgInspectionSwarmingTypeId = "Schwarmtrieb"
renderMessageGerman MsgInspectionQueenSeen = "Kö ges."
renderMessageGerman MsgInspectionTotalFrames = "Ges. Waben"
renderMessageGerman MsgInspectionBeeCoveredFrames = "Bel. Waben"
renderMessageGerman MsgInspectionBroodFrames = "Brutwaben"
renderMessageGerman MsgInspectionHoneyFrames = "Honigwaben"
renderMessageGerman MsgInspectionTreatment = "Behandlung"
renderMessageGerman MsgInspectionFeeding = "Fütterung"
renderMessageGerman MsgInspectionNotes = "Notizen"
renderMessageGerman MsgInspectionfileInspectionId = ""
renderMessageGerman MsgInspectionfileRawdataId = ""
renderMessageGerman MsgInspectionfileFilename = "Dateiname"
renderMessageGerman MsgInspectionfileMimetype = "MIME Type"
renderMessageGerman MsgInspectionfileSize = "Groesse"
renderMessageGerman MsgInspectionfileFile = "Datei"
renderMessageGerman MsgTemperTypeName = "Name"
renderMessageGerman MsgTemperTypeSortIndex = "Sortierungs-Index"
renderMessageGerman MsgRunningTypeName = "Name"
renderMessageGerman MsgRunningTypeSortIndex = "Sortierungs-Index"
renderMessageGerman MsgSwarmingTypeName = "Name"
renderMessageGerman MsgSwarmingTypeSortIndex = "Sortierungs-Index"

renderMessageEnglish :: AppMessage -> Text
renderMessageEnglish MsgGlobalHome = "Home"
renderMessageEnglish MsgGlobalAdmin = "Admin"
renderMessageEnglish MsgGlobalLogout = "Logout"
renderMessageEnglish MsgGlobalLanguage = "Language"
renderMessageEnglish MsgGlobalMyProfile = "My Profile"
renderMessageEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderMessageEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderMessageEnglish MsgGlobalUsers = "Users"
renderMessageEnglish MsgGlobalAddUser = "Add user"
renderMessageEnglish MsgGlobalEditUser = "Edit user"
renderMessageEnglish MsgGlobalDeleteUser = "Delete user"
renderMessageEnglish MsgGlobalConfigurations = "Configurations"
renderMessageEnglish MsgGlobalEditConfig = "Edit config"
renderMessageEnglish MsgGlobalTestMail = "Test-Mail"
renderMessageEnglish MsgGlobalSendTestMail = "Send Test-Mail..."
renderMessageEnglish MsgGlobalCancel = "Cancel"
renderMessageEnglish MsgGlobalLocation = "Location"
renderMessageEnglish MsgGlobalLocations = "Locations"
renderMessageEnglish MsgGlobalAddLocation = "Add location"
renderMessageEnglish MsgGlobalEditLocation = "Edit location"
renderMessageEnglish MsgGlobalDeleteLocation = "Delete location"
renderMessageEnglish MsgGlobalLocationMasterData = "Location data"
renderMessageEnglish MsgGlobalHive = "Hive"
renderMessageEnglish MsgGlobalHives = "Hives"
renderMessageEnglish MsgGlobalAddHive = "Add hive"
renderMessageEnglish MsgGlobalDeleteHive = "Delete hive"
renderMessageEnglish MsgGlobalDetailHive = "Hive details"
renderMessageEnglish MsgGlobalEditHive = "Edit hive"
renderMessageEnglish MsgGlobalHiveMasterData = "Hive data"
renderMessageEnglish MsgGlobalInspection = "Inspection"
renderMessageEnglish MsgGlobalInspectionsAll = "Inspections (all)"
renderMessageEnglish MsgGlobalInspectionsLast10 = "Inspections (last 10)"
renderMessageEnglish MsgGlobalAddInspection = "Add inspection"
renderMessageEnglish MsgGlobalDeleteInspection = "Delete inspection"
renderMessageEnglish MsgGlobalEditInspection = "Edit inspection"
renderMessageEnglish MsgGlobalLastInspection = "Last inspection"
renderMessageEnglish MsgGlobalInspectionfiles = "Files"
renderMessageEnglish MsgGlobalAddInspectionfile = "Add inspection-file"
renderMessageEnglish MsgGlobalDeleteInspectionfile = "Delete inspection-file"
renderMessageEnglish MsgGlobalEditInspectionfile = "Edit inspection-file"
renderMessageEnglish MsgGlobalTemper = "Temper"
renderMessageEnglish MsgGlobalTemperTypes = "Temper types"
renderMessageEnglish MsgGlobalAddTemperType = "Add temper type"
renderMessageEnglish MsgGlobalDeleteTemperType = "Delete temper type"
renderMessageEnglish MsgGlobalEditTemperType = "Edit temper type"
renderMessageEnglish MsgGlobalRunning = "Running beh."
renderMessageEnglish MsgGlobalRunningTypes = "Running types"
renderMessageEnglish MsgGlobalAddRunningType = "Add running type"
renderMessageEnglish MsgGlobalDeleteRunningType = "Delete running type"
renderMessageEnglish MsgGlobalEditRunningType = "Edit running type"
renderMessageEnglish MsgGlobalSwarming = "Swarming mood"
renderMessageEnglish MsgGlobalSwarmingTypes = "Swarming types"
renderMessageEnglish MsgGlobalAddSwarmingType = "Add swarming type"
renderMessageEnglish MsgGlobalDeleteSwarmingType = "Delete swarming type"
renderMessageEnglish MsgGlobalEditSwarmingType = "Edit swarming type"
renderMessageEnglish MsgUserIdent = "Login"
renderMessageEnglish MsgUserPassword = "Password"
renderMessageEnglish MsgUserEmail = "Email"
renderMessageEnglish MsgUserIsAdmin = "Is admin?"
renderMessageEnglish MsgUserIsResetPassword = "Generate new password? (Will be sent by email)"
renderMessageEnglish MsgConfigCode = "Code"
renderMessageEnglish MsgConfigStringValue = "String-Value"
renderMessageEnglish MsgConfigIntValue = "Integer-Value"
renderMessageEnglish MsgConfigDoubleValue = "Double-Value"
renderMessageEnglish MsgConfigBoolValue = "Boolean-Value"
renderMessageEnglish MsgTestmailEmail = "Email"
renderMessageEnglish MsgRawdataBytes = "Bytes"
renderMessageEnglish MsgLocationName = "Name"
renderMessageEnglish MsgHiveLocationId = ""
renderMessageEnglish MsgHiveName = "Name"
renderMessageEnglish MsgHiveDescription = "Description"
renderMessageEnglish MsgInspectionHiveId = ""
renderMessageEnglish MsgInspectionDate = "Date"
renderMessageEnglish MsgInspectionTemperTypeId = "Temper"
renderMessageEnglish MsgInspectionRunningTypeId = "Running Beh."
renderMessageEnglish MsgInspectionSwarmingTypeId = "swarming Mood"
renderMessageEnglish MsgInspectionQueenSeen = "Queen seen"
renderMessageEnglish MsgInspectionTotalFrames = "Total frames"
renderMessageEnglish MsgInspectionBeeCoveredFrames = "Bee covered frames"
renderMessageEnglish MsgInspectionBroodFrames = "Brood frames"
renderMessageEnglish MsgInspectionHoneyFrames = "Honey frames"
renderMessageEnglish MsgInspectionTreatment = "Treatment"
renderMessageEnglish MsgInspectionFeeding = "Feeding"
renderMessageEnglish MsgInspectionNotes = "Notes"
renderMessageEnglish MsgInspectionfileInspectionId = ""
renderMessageEnglish MsgInspectionfileRawdataId = ""
renderMessageEnglish MsgInspectionfileFilename = "Filename"
renderMessageEnglish MsgInspectionfileMimetype = "MIME Type"
renderMessageEnglish MsgInspectionfileSize = "Size"
renderMessageEnglish MsgInspectionfileFile = "File"
renderMessageEnglish MsgTemperTypeName = "Name"
renderMessageEnglish MsgTemperTypeSortIndex = "Sort Index"
renderMessageEnglish MsgRunningTypeName = "Name"
renderMessageEnglish MsgRunningTypeSortIndex = "Sort Index"
renderMessageEnglish MsgSwarmingTypeName = "Name"
renderMessageEnglish MsgSwarmingTypeSortIndex = "Sort Index"

data Translation = Translation
  { msgGlobalHome :: Text
  , msgGlobalAdmin :: Text
  , msgGlobalLogout :: Text
  , msgGlobalLanguage :: Text
  , msgGlobalMyProfile :: Text
  , msgGlobalEditMyProfile :: Text
  , msgGlobalReallyDelete :: Text
  , msgGlobalUsers :: Text
  , msgGlobalAddUser :: Text
  , msgGlobalEditUser :: Text
  , msgGlobalDeleteUser :: Text
  , msgGlobalConfigurations :: Text
  , msgGlobalEditConfig :: Text
  , msgGlobalTestMail :: Text
  , msgGlobalSendTestMail :: Text
  , msgGlobalCancel :: Text
  , msgGlobalLocation :: Text
  , msgGlobalLocations :: Text
  , msgGlobalAddLocation :: Text
  , msgGlobalEditLocation :: Text
  , msgGlobalDeleteLocation :: Text
  , msgGlobalLocationMasterData :: Text
  , msgGlobalHive :: Text
  , msgGlobalHives :: Text
  , msgGlobalAddHive :: Text
  , msgGlobalDeleteHive :: Text
  , msgGlobalDetailHive :: Text
  , msgGlobalEditHive :: Text
  , msgGlobalHiveMasterData :: Text
  , msgGlobalInspection :: Text
  , msgGlobalInspectionsAll :: Text
  , msgGlobalInspectionsLast10 :: Text
  , msgGlobalAddInspection :: Text
  , msgGlobalDeleteInspection :: Text
  , msgGlobalEditInspection :: Text
  , msgGlobalLastInspection :: Text
  , msgGlobalInspectionfiles :: Text
  , msgGlobalAddInspectionfile :: Text
  , msgGlobalDeleteInspectionfile :: Text
  , msgGlobalEditInspectionfile :: Text
  , msgGlobalTemper :: Text
  , msgGlobalTemperTypes :: Text
  , msgGlobalAddTemperType :: Text
  , msgGlobalDeleteTemperType :: Text
  , msgGlobalEditTemperType :: Text
  , msgGlobalRunning :: Text
  , msgGlobalRunningTypes :: Text
  , msgGlobalAddRunningType :: Text
  , msgGlobalDeleteRunningType :: Text
  , msgGlobalEditRunningType :: Text
  , msgGlobalSwarming :: Text
  , msgGlobalSwarmingTypes :: Text
  , msgGlobalAddSwarmingType :: Text
  , msgGlobalDeleteSwarmingType :: Text
  , msgGlobalEditSwarmingType :: Text
  , msgUserIdent :: Text
  , msgUserPassword :: Text
  , msgUserEmail :: Text
  , msgUserIsAdmin :: Text
  , msgUserIsResetPassword :: Text
  , msgConfigCode :: Text
  , msgConfigStringValue :: Text
  , msgConfigIntValue :: Text
  , msgConfigDoubleValue :: Text
  , msgConfigBoolValue :: Text
  , msgTestmailEmail :: Text
  , msgRawdataBytes :: Text
  , msgLocationName :: Text
  , msgHiveLocationId :: Text
  , msgHiveName :: Text
  , msgHiveDescription :: Text
  , msgInspectionHiveId :: Text
  , msgInspectionDate :: Text
  , msgInspectionTemperTypeId :: Text
  , msgInspectionRunningTypeId :: Text
  , msgInspectionSwarmingTypeId :: Text
  , msgInspectionQueenSeen :: Text
  , msgInspectionTotalFrames :: Text
  , msgInspectionBeeCoveredFrames :: Text
  , msgInspectionBroodFrames :: Text
  , msgInspectionHoneyFrames :: Text
  , msgInspectionTreatment :: Text
  , msgInspectionFeeding :: Text
  , msgInspectionNotes :: Text
  , msgInspectionfileInspectionId :: Text
  , msgInspectionfileRawdataId :: Text
  , msgInspectionfileFilename :: Text
  , msgInspectionfileMimetype :: Text
  , msgInspectionfileSize :: Text
  , msgInspectionfileFile :: Text
  , msgTemperTypeName :: Text
  , msgTemperTypeSortIndex :: Text
  , msgRunningTypeName :: Text
  , msgRunningTypeSortIndex :: Text
  , msgSwarmingTypeName :: Text
  , msgSwarmingTypeSortIndex :: Text
  } deriving Generic

instance ToJSON Translation

translationDe :: Translation
translationDe = Translation
  { msgGlobalHome = "Home"
  , msgGlobalAdmin = "Admin"
  , msgGlobalLogout = "Logout"
  , msgGlobalLanguage = "Sprache"
  , msgGlobalMyProfile = "Mein Profil"
  , msgGlobalEditMyProfile = "Mein Profil bearbeiten"
  , msgGlobalReallyDelete = "Möchten sie wirklich löschen?"
  , msgGlobalUsers = "Nutzer"
  , msgGlobalAddUser = "Nutzer hinzufügen"
  , msgGlobalEditUser = "Nutzer bearbeiten"
  , msgGlobalDeleteUser = "Nutzer löschen"
  , msgGlobalConfigurations = "Konfigurationen"
  , msgGlobalEditConfig = "Konfiguration bearbeiten"
  , msgGlobalTestMail = "Test-Mail"
  , msgGlobalSendTestMail = "Test-Mail senden..."
  , msgGlobalCancel = "Abbrechen"
  , msgGlobalLocation = "Standort"
  , msgGlobalLocations = "Standorte"
  , msgGlobalAddLocation = "Standort hinzufügen"
  , msgGlobalEditLocation = "Standort bearbeiten"
  , msgGlobalDeleteLocation = "Standort löschen"
  , msgGlobalLocationMasterData = "Standort-Daten"
  , msgGlobalHive = "Bienenstock"
  , msgGlobalHives = "Bienenstöcke"
  , msgGlobalAddHive = "Bienenstock hinzufügen"
  , msgGlobalDeleteHive = "Bienenstock löschen"
  , msgGlobalDetailHive = "Bienenstock Details"
  , msgGlobalEditHive = "Bienenstock bearbeiten"
  , msgGlobalHiveMasterData = "Stock-Daten"
  , msgGlobalInspection = "Durchsicht"
  , msgGlobalInspectionsAll = "Durchsichten (alle)"
  , msgGlobalInspectionsLast10 = "Durchsichten (letzten 10)"
  , msgGlobalAddInspection = "Durchsicht hinzufügen"
  , msgGlobalDeleteInspection = "Durchsicht löschen"
  , msgGlobalEditInspection = "Durchsicht bearbeiten"
  , msgGlobalLastInspection = "Letzte Durchsicht"
  , msgGlobalInspectionfiles = "Dateien"
  , msgGlobalAddInspectionfile = "Durchsicht-Datei hinzufügen"
  , msgGlobalDeleteInspectionfile = "Durchsicht-Datei löschen"
  , msgGlobalEditInspectionfile = "Durchsicht-Datei bearbeiten"
  , msgGlobalTemper = "Sanftmut"
  , msgGlobalTemperTypes = "Sanftmut Typen"
  , msgGlobalAddTemperType = "Sanftmut Typ hinzufügen"
  , msgGlobalDeleteTemperType = "Sanftmut Typ löschen"
  , msgGlobalEditTemperType = "Sanftmut Typ bearbeiten"
  , msgGlobalRunning = "Wabensitz"
  , msgGlobalRunningTypes = "Wabensitz Typen"
  , msgGlobalAddRunningType = "Wabensitz Typ hinzufügen"
  , msgGlobalDeleteRunningType = "Wabensitz Typ löschen"
  , msgGlobalEditRunningType = "Wabensitz Typ bearbeiten"
  , msgGlobalSwarming = "Schwarmtrieb"
  , msgGlobalSwarmingTypes = "Schwarmtrieb Typen"
  , msgGlobalAddSwarmingType = "Schwarmtrieb Typ hinzufügen"
  , msgGlobalDeleteSwarmingType = "Schwarmtrieb Typ löschen"
  , msgGlobalEditSwarmingType = "Schwarmtrieb Typ bearbeiten"
  , msgUserIdent = "Login"
  , msgUserPassword = "Passwort"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Ist Admin?"
  , msgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Wert"
  , msgConfigIntValue = "Integer-Wert"
  , msgConfigDoubleValue = "Double-Wert"
  , msgConfigBoolValue = "Boolean-Wert"
  , msgTestmailEmail = "Email"
  , msgRawdataBytes = "Bytes"
  , msgLocationName = "Name"
  , msgHiveLocationId = ""
  , msgHiveName = "Name"
  , msgHiveDescription = "Beschreibung"
  , msgInspectionHiveId = ""
  , msgInspectionDate = "Datum"
  , msgInspectionTemperTypeId = "Sanftmut"
  , msgInspectionRunningTypeId = "Wabensitz"
  , msgInspectionSwarmingTypeId = "Schwarmtrieb"
  , msgInspectionQueenSeen = "Kö ges."
  , msgInspectionTotalFrames = "Ges. Waben"
  , msgInspectionBeeCoveredFrames = "Bel. Waben"
  , msgInspectionBroodFrames = "Brutwaben"
  , msgInspectionHoneyFrames = "Honigwaben"
  , msgInspectionTreatment = "Behandlung"
  , msgInspectionFeeding = "Fütterung"
  , msgInspectionNotes = "Notizen"
  , msgInspectionfileInspectionId = ""
  , msgInspectionfileRawdataId = ""
  , msgInspectionfileFilename = "Dateiname"
  , msgInspectionfileMimetype = "MIME Type"
  , msgInspectionfileSize = "Groesse"
  , msgInspectionfileFile = "Datei"
  , msgTemperTypeName = "Name"
  , msgTemperTypeSortIndex = "Sortierungs-Index"
  , msgRunningTypeName = "Name"
  , msgRunningTypeSortIndex = "Sortierungs-Index"
  , msgSwarmingTypeName = "Name"
  , msgSwarmingTypeSortIndex = "Sortierungs-Index"}

translationEn :: Translation
translationEn = Translation
  { msgGlobalHome = "Home"
  , msgGlobalAdmin = "Admin"
  , msgGlobalLogout = "Logout"
  , msgGlobalLanguage = "Language"
  , msgGlobalMyProfile = "My Profile"
  , msgGlobalEditMyProfile = "Edit my profile"
  , msgGlobalReallyDelete = "Are you sure to delete?"
  , msgGlobalUsers = "Users"
  , msgGlobalAddUser = "Add user"
  , msgGlobalEditUser = "Edit user"
  , msgGlobalDeleteUser = "Delete user"
  , msgGlobalConfigurations = "Configurations"
  , msgGlobalEditConfig = "Edit config"
  , msgGlobalTestMail = "Test-Mail"
  , msgGlobalSendTestMail = "Send Test-Mail..."
  , msgGlobalCancel = "Cancel"
  , msgGlobalLocation = "Location"
  , msgGlobalLocations = "Locations"
  , msgGlobalAddLocation = "Add location"
  , msgGlobalEditLocation = "Edit location"
  , msgGlobalDeleteLocation = "Delete location"
  , msgGlobalLocationMasterData = "Location data"
  , msgGlobalHive = "Hive"
  , msgGlobalHives = "Hives"
  , msgGlobalAddHive = "Add hive"
  , msgGlobalDeleteHive = "Delete hive"
  , msgGlobalDetailHive = "Hive details"
  , msgGlobalEditHive = "Edit hive"
  , msgGlobalHiveMasterData = "Hive data"
  , msgGlobalInspection = "Inspection"
  , msgGlobalInspectionsAll = "Inspections (all)"
  , msgGlobalInspectionsLast10 = "Inspections (last 10)"
  , msgGlobalAddInspection = "Add inspection"
  , msgGlobalDeleteInspection = "Delete inspection"
  , msgGlobalEditInspection = "Edit inspection"
  , msgGlobalLastInspection = "Last inspection"
  , msgGlobalInspectionfiles = "Files"
  , msgGlobalAddInspectionfile = "Add inspection-file"
  , msgGlobalDeleteInspectionfile = "Delete inspection-file"
  , msgGlobalEditInspectionfile = "Edit inspection-file"
  , msgGlobalTemper = "Temper"
  , msgGlobalTemperTypes = "Temper types"
  , msgGlobalAddTemperType = "Add temper type"
  , msgGlobalDeleteTemperType = "Delete temper type"
  , msgGlobalEditTemperType = "Edit temper type"
  , msgGlobalRunning = "Running beh."
  , msgGlobalRunningTypes = "Running types"
  , msgGlobalAddRunningType = "Add running type"
  , msgGlobalDeleteRunningType = "Delete running type"
  , msgGlobalEditRunningType = "Edit running type"
  , msgGlobalSwarming = "Swarming mood"
  , msgGlobalSwarmingTypes = "Swarming types"
  , msgGlobalAddSwarmingType = "Add swarming type"
  , msgGlobalDeleteSwarmingType = "Delete swarming type"
  , msgGlobalEditSwarmingType = "Edit swarming type"
  , msgUserIdent = "Login"
  , msgUserPassword = "Password"
  , msgUserEmail = "Email"
  , msgUserIsAdmin = "Is admin?"
  , msgUserIsResetPassword = "Generate new password? (Will be sent by email)"
  , msgConfigCode = "Code"
  , msgConfigStringValue = "String-Value"
  , msgConfigIntValue = "Integer-Value"
  , msgConfigDoubleValue = "Double-Value"
  , msgConfigBoolValue = "Boolean-Value"
  , msgTestmailEmail = "Email"
  , msgRawdataBytes = "Bytes"
  , msgLocationName = "Name"
  , msgHiveLocationId = ""
  , msgHiveName = "Name"
  , msgHiveDescription = "Description"
  , msgInspectionHiveId = ""
  , msgInspectionDate = "Date"
  , msgInspectionTemperTypeId = "Temper"
  , msgInspectionRunningTypeId = "Running Beh."
  , msgInspectionSwarmingTypeId = "swarming Mood"
  , msgInspectionQueenSeen = "Queen seen"
  , msgInspectionTotalFrames = "Total frames"
  , msgInspectionBeeCoveredFrames = "Bee covered frames"
  , msgInspectionBroodFrames = "Brood frames"
  , msgInspectionHoneyFrames = "Honey frames"
  , msgInspectionTreatment = "Treatment"
  , msgInspectionFeeding = "Feeding"
  , msgInspectionNotes = "Notes"
  , msgInspectionfileInspectionId = ""
  , msgInspectionfileRawdataId = ""
  , msgInspectionfileFilename = "Filename"
  , msgInspectionfileMimetype = "MIME Type"
  , msgInspectionfileSize = "Size"
  , msgInspectionfileFile = "File"
  , msgTemperTypeName = "Name"
  , msgTemperTypeSortIndex = "Sort Index"
  , msgRunningTypeName = "Name"
  , msgRunningTypeSortIndex = "Sort Index"
  , msgSwarmingTypeName = "Name"
  , msgSwarmingTypeSortIndex = "Sort Index"}

-- gen i18n - end