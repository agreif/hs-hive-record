{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module I18n where

import ClassyPrelude.Yesod

-- gen i18n - start
data AppMessage
  = MsgGlobalHome
  | MsgGlobalAdmin
  | MsgGlobalLogout
  | MsgGlobalLanguage
  | MsgGlobalMyProfile
  | MsgGlobalEditMyProfile
  | MsgGlobalReallyDelete
  | MsgGlobalShowMore
  | MsgGlobalShowLess
  | MsgGlobalShowAll
  | MsgGlobalUsers
  | MsgGlobalAddUser
  | MsgGlobalEditUser
  | MsgGlobalDeleteUser
  | MsgGlobalConfigurations
  | MsgGlobalEditConfig
  | MsgGlobalCancel
  | MsgGlobalInfo
  | MsgGlobalLocation
  | MsgGlobalLocations
  | MsgGlobalAddLocation
  | MsgGlobalEditLocation
  | MsgGlobalDeleteLocation
  | MsgGlobalLocationMasterData
  | MsgGlobalHiveOverview
  | MsgGlobalHive
  | MsgGlobalHives
  | MsgGlobalAddHive
  | MsgGlobalDeleteHive
  | MsgGlobalDetailHive
  | MsgGlobalEditHive
  | MsgGlobalHiveMasterData
  | MsgGlobalInspection
  | MsgGlobalInspections
  | MsgGlobalAddInspection
  | MsgGlobalDeleteInspection
  | MsgGlobalEditInspection
  | MsgGlobalLastInspection
  | MsgGlobalInspectionfiles
  | MsgGlobalAddInspectionfile
  | MsgGlobalDeleteInspectionfile
  | MsgGlobalEditInspectionfile
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
  | MsgRawdataBytes
  | MsgLocationName
  | MsgHiveLocationId
  | MsgHiveName
  | MsgHiveQueenYear
  | MsgHiveDescription
  | MsgHiveIsDissolved
  | MsgInspectionHiveId
  | MsgInspectionDate
  | MsgInspectionSwarmingTypeId
  | MsgInspectionQueenSeen
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
  | MsgSwarmingTypeName
  | MsgSwarmingTypeSortIndex
  | MsgUserIdentInputInfo
  | MsgUserPasswordInputInfo
  | MsgUserEmailInputInfo
  | MsgUserIsAdminInputInfo
  | MsgUserIsResetPasswordInputInfo
  | MsgConfigCodeInputInfo
  | MsgConfigStringValueInputInfo
  | MsgConfigIntValueInputInfo
  | MsgConfigDoubleValueInputInfo
  | MsgConfigBoolValueInputInfo
  | MsgRawdataBytesInputInfo
  | MsgLocationNameInputInfo
  | MsgHiveLocationIdInputInfo
  | MsgHiveNameInputInfo
  | MsgHiveQueenYearInputInfo
  | MsgHiveDescriptionInputInfo
  | MsgHiveIsDissolvedInputInfo
  | MsgInspectionHiveIdInputInfo
  | MsgInspectionDateInputInfo
  | MsgInspectionSwarmingTypeIdInputInfo
  | MsgInspectionQueenSeenInputInfo
  | MsgInspectionBeeCoveredFramesInputInfo
  | MsgInspectionBroodFramesInputInfo
  | MsgInspectionHoneyFramesInputInfo
  | MsgInspectionTreatmentInputInfo
  | MsgInspectionFeedingInputInfo
  | MsgInspectionNotesInputInfo
  | MsgInspectionfileInspectionIdInputInfo
  | MsgInspectionfileRawdataIdInputInfo
  | MsgInspectionfileFilenameInputInfo
  | MsgInspectionfileMimetypeInputInfo
  | MsgInspectionfileSizeInputInfo
  | MsgInspectionfileFileInputInfo
  | MsgSwarmingTypeNameInputInfo
  | MsgSwarmingTypeSortIndexInputInfo
  | MsgTestmailEmail
  | MsgTestmailEmailInputInfo
  | MsgTestmailTestMail
  | MsgTestmailSendTestMail

renderMessageGerman :: AppMessage -> Text
renderMessageGerman MsgGlobalHome = "Home"
renderMessageGerman MsgGlobalAdmin = "Admin"
renderMessageGerman MsgGlobalLogout = "Logout"
renderMessageGerman MsgGlobalLanguage = "Sprache"
renderMessageGerman MsgGlobalMyProfile = "Mein Profil"
renderMessageGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderMessageGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderMessageGerman MsgGlobalShowMore = "Mehr anzeigen"
renderMessageGerman MsgGlobalShowLess = "Weniger anzeigen"
renderMessageGerman MsgGlobalShowAll = "Alle anzeigen"
renderMessageGerman MsgGlobalUsers = "Nutzer"
renderMessageGerman MsgGlobalAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgGlobalEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgGlobalDeleteUser = "Nutzer löschen"
renderMessageGerman MsgGlobalConfigurations = "Konfigurationen"
renderMessageGerman MsgGlobalEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgGlobalInfo = "Info"
renderMessageGerman MsgGlobalLocation = "Standort"
renderMessageGerman MsgGlobalLocations = "Standorte"
renderMessageGerman MsgGlobalAddLocation = "Standort hinzufügen"
renderMessageGerman MsgGlobalEditLocation = "Standort bearbeiten"
renderMessageGerman MsgGlobalDeleteLocation = "Standort löschen"
renderMessageGerman MsgGlobalLocationMasterData = "Standort-Daten"
renderMessageGerman MsgGlobalHiveOverview = "Bienenstock Übersicht"
renderMessageGerman MsgGlobalHive = "Bienenstock"
renderMessageGerman MsgGlobalHives = "Bienenstöcke"
renderMessageGerman MsgGlobalAddHive = "Bienenstock hinzufügen"
renderMessageGerman MsgGlobalDeleteHive = "Bienenstock löschen"
renderMessageGerman MsgGlobalDetailHive = "Bienenstock Details"
renderMessageGerman MsgGlobalEditHive = "Bienenstock bearbeiten"
renderMessageGerman MsgGlobalHiveMasterData = "Stock-Daten"
renderMessageGerman MsgGlobalInspection = "Durchsicht"
renderMessageGerman MsgGlobalInspections = "Durchsichten"
renderMessageGerman MsgGlobalAddInspection = "Durchsicht hinzufügen"
renderMessageGerman MsgGlobalDeleteInspection = "Durchsicht löschen"
renderMessageGerman MsgGlobalEditInspection = "Durchsicht bearbeiten"
renderMessageGerman MsgGlobalLastInspection = "Letzte Durchsicht"
renderMessageGerman MsgGlobalInspectionfiles = "Dateien"
renderMessageGerman MsgGlobalAddInspectionfile = "Durchsicht-Datei hinzufügen"
renderMessageGerman MsgGlobalDeleteInspectionfile = "Durchsicht-Datei löschen"
renderMessageGerman MsgGlobalEditInspectionfile = "Durchsicht-Datei bearbeiten"
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
renderMessageGerman MsgRawdataBytes = "Bytes"
renderMessageGerman MsgLocationName = "Name"
renderMessageGerman MsgHiveLocationId = "Standort"
renderMessageGerman MsgHiveName = "Name"
renderMessageGerman MsgHiveQueenYear = "Königin Jahr"
renderMessageGerman MsgHiveDescription = "Beschreibung"
renderMessageGerman MsgHiveIsDissolved = "Ist aufgelöst?"
renderMessageGerman MsgInspectionHiveId = ""
renderMessageGerman MsgInspectionDate = "Datum"
renderMessageGerman MsgInspectionSwarmingTypeId = "Schwarmtrieb"
renderMessageGerman MsgInspectionQueenSeen = "Kö ges."
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
renderMessageGerman MsgSwarmingTypeName = "Name"
renderMessageGerman MsgSwarmingTypeSortIndex = "Sortierungs-Index"
renderMessageGerman MsgUserIdentInputInfo = ""
renderMessageGerman MsgUserPasswordInputInfo = ""
renderMessageGerman MsgUserEmailInputInfo = ""
renderMessageGerman MsgUserIsAdminInputInfo = ""
renderMessageGerman MsgUserIsResetPasswordInputInfo = ""
renderMessageGerman MsgConfigCodeInputInfo = ""
renderMessageGerman MsgConfigStringValueInputInfo = ""
renderMessageGerman MsgConfigIntValueInputInfo = ""
renderMessageGerman MsgConfigDoubleValueInputInfo = ""
renderMessageGerman MsgConfigBoolValueInputInfo = ""
renderMessageGerman MsgRawdataBytesInputInfo = ""
renderMessageGerman MsgLocationNameInputInfo = ""
renderMessageGerman MsgHiveLocationIdInputInfo = ""
renderMessageGerman MsgHiveNameInputInfo = ""
renderMessageGerman MsgHiveQueenYearInputInfo = ""
renderMessageGerman MsgHiveDescriptionInputInfo = ""
renderMessageGerman MsgHiveIsDissolvedInputInfo = ""
renderMessageGerman MsgInspectionHiveIdInputInfo = ""
renderMessageGerman MsgInspectionDateInputInfo = ""
renderMessageGerman MsgInspectionSwarmingTypeIdInputInfo = ""
renderMessageGerman MsgInspectionQueenSeenInputInfo = ""
renderMessageGerman MsgInspectionBeeCoveredFramesInputInfo = ""
renderMessageGerman MsgInspectionBroodFramesInputInfo = ""
renderMessageGerman MsgInspectionHoneyFramesInputInfo = ""
renderMessageGerman MsgInspectionTreatmentInputInfo = ""
renderMessageGerman MsgInspectionFeedingInputInfo = ""
renderMessageGerman MsgInspectionNotesInputInfo = ""
renderMessageGerman MsgInspectionfileInspectionIdInputInfo = ""
renderMessageGerman MsgInspectionfileRawdataIdInputInfo = ""
renderMessageGerman MsgInspectionfileFilenameInputInfo = ""
renderMessageGerman MsgInspectionfileMimetypeInputInfo = ""
renderMessageGerman MsgInspectionfileSizeInputInfo = ""
renderMessageGerman MsgInspectionfileFileInputInfo = ""
renderMessageGerman MsgSwarmingTypeNameInputInfo = ""
renderMessageGerman MsgSwarmingTypeSortIndexInputInfo = ""
renderMessageGerman MsgTestmailEmail = "Email"
renderMessageGerman MsgTestmailEmailInputInfo = ""
renderMessageGerman MsgTestmailTestMail = "Test-Mail"
renderMessageGerman MsgTestmailSendTestMail = "Test-Mail senden..."

renderMessageEnglish :: AppMessage -> Text
renderMessageEnglish MsgGlobalHome = "Home"
renderMessageEnglish MsgGlobalAdmin = "Admin"
renderMessageEnglish MsgGlobalLogout = "Logout"
renderMessageEnglish MsgGlobalLanguage = "Language"
renderMessageEnglish MsgGlobalMyProfile = "My Profile"
renderMessageEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderMessageEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderMessageEnglish MsgGlobalShowMore = "Show More"
renderMessageEnglish MsgGlobalShowLess = "Show Less"
renderMessageEnglish MsgGlobalShowAll = "Show All"
renderMessageEnglish MsgGlobalUsers = "Users"
renderMessageEnglish MsgGlobalAddUser = "Add user"
renderMessageEnglish MsgGlobalEditUser = "Edit user"
renderMessageEnglish MsgGlobalDeleteUser = "Delete user"
renderMessageEnglish MsgGlobalConfigurations = "Configurations"
renderMessageEnglish MsgGlobalEditConfig = "Edit config"
renderMessageEnglish MsgGlobalCancel = "Cancel"
renderMessageEnglish MsgGlobalInfo = "Info"
renderMessageEnglish MsgGlobalLocation = "Location"
renderMessageEnglish MsgGlobalLocations = "Locations"
renderMessageEnglish MsgGlobalAddLocation = "Add location"
renderMessageEnglish MsgGlobalEditLocation = "Edit location"
renderMessageEnglish MsgGlobalDeleteLocation = "Delete location"
renderMessageEnglish MsgGlobalLocationMasterData = "Location data"
renderMessageEnglish MsgGlobalHiveOverview = "Hive Overview"
renderMessageEnglish MsgGlobalHive = "Hive"
renderMessageEnglish MsgGlobalHives = "Hives"
renderMessageEnglish MsgGlobalAddHive = "Add hive"
renderMessageEnglish MsgGlobalDeleteHive = "Delete hive"
renderMessageEnglish MsgGlobalDetailHive = "Hive details"
renderMessageEnglish MsgGlobalEditHive = "Edit hive"
renderMessageEnglish MsgGlobalHiveMasterData = "Hive data"
renderMessageEnglish MsgGlobalInspection = "Inspection"
renderMessageEnglish MsgGlobalInspections = "Inspections"
renderMessageEnglish MsgGlobalAddInspection = "Add inspection"
renderMessageEnglish MsgGlobalDeleteInspection = "Delete inspection"
renderMessageEnglish MsgGlobalEditInspection = "Edit inspection"
renderMessageEnglish MsgGlobalLastInspection = "Last inspection"
renderMessageEnglish MsgGlobalInspectionfiles = "Files"
renderMessageEnglish MsgGlobalAddInspectionfile = "Add inspection-file"
renderMessageEnglish MsgGlobalDeleteInspectionfile = "Delete inspection-file"
renderMessageEnglish MsgGlobalEditInspectionfile = "Edit inspection-file"
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
renderMessageEnglish MsgRawdataBytes = "Bytes"
renderMessageEnglish MsgLocationName = "Name"
renderMessageEnglish MsgHiveLocationId = "Location"
renderMessageEnglish MsgHiveName = "Name"
renderMessageEnglish MsgHiveQueenYear = "Queen Year"
renderMessageEnglish MsgHiveDescription = "Description"
renderMessageEnglish MsgHiveIsDissolved = "Is dissolved?"
renderMessageEnglish MsgInspectionHiveId = ""
renderMessageEnglish MsgInspectionDate = "Date"
renderMessageEnglish MsgInspectionSwarmingTypeId = "swarming Mood"
renderMessageEnglish MsgInspectionQueenSeen = "Queen seen"
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
renderMessageEnglish MsgSwarmingTypeName = "Name"
renderMessageEnglish MsgSwarmingTypeSortIndex = "Sort Index"
renderMessageEnglish MsgUserIdentInputInfo = ""
renderMessageEnglish MsgUserPasswordInputInfo = ""
renderMessageEnglish MsgUserEmailInputInfo = ""
renderMessageEnglish MsgUserIsAdminInputInfo = ""
renderMessageEnglish MsgUserIsResetPasswordInputInfo = ""
renderMessageEnglish MsgConfigCodeInputInfo = ""
renderMessageEnglish MsgConfigStringValueInputInfo = ""
renderMessageEnglish MsgConfigIntValueInputInfo = ""
renderMessageEnglish MsgConfigDoubleValueInputInfo = ""
renderMessageEnglish MsgConfigBoolValueInputInfo = ""
renderMessageEnglish MsgRawdataBytesInputInfo = ""
renderMessageEnglish MsgLocationNameInputInfo = ""
renderMessageEnglish MsgHiveLocationIdInputInfo = ""
renderMessageEnglish MsgHiveNameInputInfo = ""
renderMessageEnglish MsgHiveQueenYearInputInfo = ""
renderMessageEnglish MsgHiveDescriptionInputInfo = ""
renderMessageEnglish MsgHiveIsDissolvedInputInfo = ""
renderMessageEnglish MsgInspectionHiveIdInputInfo = ""
renderMessageEnglish MsgInspectionDateInputInfo = ""
renderMessageEnglish MsgInspectionSwarmingTypeIdInputInfo = ""
renderMessageEnglish MsgInspectionQueenSeenInputInfo = ""
renderMessageEnglish MsgInspectionBeeCoveredFramesInputInfo = ""
renderMessageEnglish MsgInspectionBroodFramesInputInfo = ""
renderMessageEnglish MsgInspectionHoneyFramesInputInfo = ""
renderMessageEnglish MsgInspectionTreatmentInputInfo = ""
renderMessageEnglish MsgInspectionFeedingInputInfo = ""
renderMessageEnglish MsgInspectionNotesInputInfo = ""
renderMessageEnglish MsgInspectionfileInspectionIdInputInfo = ""
renderMessageEnglish MsgInspectionfileRawdataIdInputInfo = ""
renderMessageEnglish MsgInspectionfileFilenameInputInfo = ""
renderMessageEnglish MsgInspectionfileMimetypeInputInfo = ""
renderMessageEnglish MsgInspectionfileSizeInputInfo = ""
renderMessageEnglish MsgInspectionfileFileInputInfo = ""
renderMessageEnglish MsgSwarmingTypeNameInputInfo = ""
renderMessageEnglish MsgSwarmingTypeSortIndexInputInfo = ""
renderMessageEnglish MsgTestmailEmail = "Email"
renderMessageEnglish MsgTestmailEmailInputInfo = ""
renderMessageEnglish MsgTestmailTestMail = "Test-Mail"
renderMessageEnglish MsgTestmailSendTestMail = "Send Test-Mail..."

data Translation = Translation
  { msgGlobalHome :: Text,
    msgGlobalAdmin :: Text,
    msgGlobalLogout :: Text,
    msgGlobalLanguage :: Text,
    msgGlobalMyProfile :: Text,
    msgGlobalEditMyProfile :: Text,
    msgGlobalReallyDelete :: Text,
    msgGlobalShowMore :: Text,
    msgGlobalShowLess :: Text,
    msgGlobalShowAll :: Text,
    msgGlobalUsers :: Text,
    msgGlobalAddUser :: Text,
    msgGlobalEditUser :: Text,
    msgGlobalDeleteUser :: Text,
    msgGlobalConfigurations :: Text,
    msgGlobalEditConfig :: Text,
    msgGlobalCancel :: Text,
    msgGlobalInfo :: Text,
    msgGlobalLocation :: Text,
    msgGlobalLocations :: Text,
    msgGlobalAddLocation :: Text,
    msgGlobalEditLocation :: Text,
    msgGlobalDeleteLocation :: Text,
    msgGlobalLocationMasterData :: Text,
    msgGlobalHiveOverview :: Text,
    msgGlobalHive :: Text,
    msgGlobalHives :: Text,
    msgGlobalAddHive :: Text,
    msgGlobalDeleteHive :: Text,
    msgGlobalDetailHive :: Text,
    msgGlobalEditHive :: Text,
    msgGlobalHiveMasterData :: Text,
    msgGlobalInspection :: Text,
    msgGlobalInspections :: Text,
    msgGlobalAddInspection :: Text,
    msgGlobalDeleteInspection :: Text,
    msgGlobalEditInspection :: Text,
    msgGlobalLastInspection :: Text,
    msgGlobalInspectionfiles :: Text,
    msgGlobalAddInspectionfile :: Text,
    msgGlobalDeleteInspectionfile :: Text,
    msgGlobalEditInspectionfile :: Text,
    msgGlobalSwarming :: Text,
    msgGlobalSwarmingTypes :: Text,
    msgGlobalAddSwarmingType :: Text,
    msgGlobalDeleteSwarmingType :: Text,
    msgGlobalEditSwarmingType :: Text,
    msgUserIdent :: Text,
    msgUserPassword :: Text,
    msgUserEmail :: Text,
    msgUserIsAdmin :: Text,
    msgUserIsResetPassword :: Text,
    msgConfigCode :: Text,
    msgConfigStringValue :: Text,
    msgConfigIntValue :: Text,
    msgConfigDoubleValue :: Text,
    msgConfigBoolValue :: Text,
    msgRawdataBytes :: Text,
    msgLocationName :: Text,
    msgHiveLocationId :: Text,
    msgHiveName :: Text,
    msgHiveQueenYear :: Text,
    msgHiveDescription :: Text,
    msgHiveIsDissolved :: Text,
    msgInspectionHiveId :: Text,
    msgInspectionDate :: Text,
    msgInspectionSwarmingTypeId :: Text,
    msgInspectionQueenSeen :: Text,
    msgInspectionBeeCoveredFrames :: Text,
    msgInspectionBroodFrames :: Text,
    msgInspectionHoneyFrames :: Text,
    msgInspectionTreatment :: Text,
    msgInspectionFeeding :: Text,
    msgInspectionNotes :: Text,
    msgInspectionfileInspectionId :: Text,
    msgInspectionfileRawdataId :: Text,
    msgInspectionfileFilename :: Text,
    msgInspectionfileMimetype :: Text,
    msgInspectionfileSize :: Text,
    msgInspectionfileFile :: Text,
    msgSwarmingTypeName :: Text,
    msgSwarmingTypeSortIndex :: Text,
    msgUserIdentInputInfo :: Text,
    msgUserPasswordInputInfo :: Text,
    msgUserEmailInputInfo :: Text,
    msgUserIsAdminInputInfo :: Text,
    msgUserIsResetPasswordInputInfo :: Text,
    msgConfigCodeInputInfo :: Text,
    msgConfigStringValueInputInfo :: Text,
    msgConfigIntValueInputInfo :: Text,
    msgConfigDoubleValueInputInfo :: Text,
    msgConfigBoolValueInputInfo :: Text,
    msgRawdataBytesInputInfo :: Text,
    msgLocationNameInputInfo :: Text,
    msgHiveLocationIdInputInfo :: Text,
    msgHiveNameInputInfo :: Text,
    msgHiveQueenYearInputInfo :: Text,
    msgHiveDescriptionInputInfo :: Text,
    msgHiveIsDissolvedInputInfo :: Text,
    msgInspectionHiveIdInputInfo :: Text,
    msgInspectionDateInputInfo :: Text,
    msgInspectionSwarmingTypeIdInputInfo :: Text,
    msgInspectionQueenSeenInputInfo :: Text,
    msgInspectionBeeCoveredFramesInputInfo :: Text,
    msgInspectionBroodFramesInputInfo :: Text,
    msgInspectionHoneyFramesInputInfo :: Text,
    msgInspectionTreatmentInputInfo :: Text,
    msgInspectionFeedingInputInfo :: Text,
    msgInspectionNotesInputInfo :: Text,
    msgInspectionfileInspectionIdInputInfo :: Text,
    msgInspectionfileRawdataIdInputInfo :: Text,
    msgInspectionfileFilenameInputInfo :: Text,
    msgInspectionfileMimetypeInputInfo :: Text,
    msgInspectionfileSizeInputInfo :: Text,
    msgInspectionfileFileInputInfo :: Text,
    msgSwarmingTypeNameInputInfo :: Text,
    msgSwarmingTypeSortIndexInputInfo :: Text,
    msgTestmailEmail :: Text,
    msgTestmailEmailInputInfo :: Text,
    msgTestmailTestMail :: Text,
    msgTestmailSendTestMail :: Text
  }
  deriving (Generic)

instance ToJSON Translation

translationDe :: Translation
translationDe =
  Translation
    { msgGlobalHome = "Home",
      msgGlobalAdmin = "Admin",
      msgGlobalLogout = "Logout",
      msgGlobalLanguage = "Sprache",
      msgGlobalMyProfile = "Mein Profil",
      msgGlobalEditMyProfile = "Mein Profil bearbeiten",
      msgGlobalReallyDelete = "Möchten sie wirklich löschen?",
      msgGlobalShowMore = "Mehr anzeigen",
      msgGlobalShowLess = "Weniger anzeigen",
      msgGlobalShowAll = "Alle anzeigen",
      msgGlobalUsers = "Nutzer",
      msgGlobalAddUser = "Nutzer hinzufügen",
      msgGlobalEditUser = "Nutzer bearbeiten",
      msgGlobalDeleteUser = "Nutzer löschen",
      msgGlobalConfigurations = "Konfigurationen",
      msgGlobalEditConfig = "Konfiguration bearbeiten",
      msgGlobalCancel = "Abbrechen",
      msgGlobalInfo = "Info",
      msgGlobalLocation = "Standort",
      msgGlobalLocations = "Standorte",
      msgGlobalAddLocation = "Standort hinzufügen",
      msgGlobalEditLocation = "Standort bearbeiten",
      msgGlobalDeleteLocation = "Standort löschen",
      msgGlobalLocationMasterData = "Standort-Daten",
      msgGlobalHiveOverview = "Bienenstock Übersicht",
      msgGlobalHive = "Bienenstock",
      msgGlobalHives = "Bienenstöcke",
      msgGlobalAddHive = "Bienenstock hinzufügen",
      msgGlobalDeleteHive = "Bienenstock löschen",
      msgGlobalDetailHive = "Bienenstock Details",
      msgGlobalEditHive = "Bienenstock bearbeiten",
      msgGlobalHiveMasterData = "Stock-Daten",
      msgGlobalInspection = "Durchsicht",
      msgGlobalInspections = "Durchsichten",
      msgGlobalAddInspection = "Durchsicht hinzufügen",
      msgGlobalDeleteInspection = "Durchsicht löschen",
      msgGlobalEditInspection = "Durchsicht bearbeiten",
      msgGlobalLastInspection = "Letzte Durchsicht",
      msgGlobalInspectionfiles = "Dateien",
      msgGlobalAddInspectionfile = "Durchsicht-Datei hinzufügen",
      msgGlobalDeleteInspectionfile = "Durchsicht-Datei löschen",
      msgGlobalEditInspectionfile = "Durchsicht-Datei bearbeiten",
      msgGlobalSwarming = "Schwarmtrieb",
      msgGlobalSwarmingTypes = "Schwarmtrieb Typen",
      msgGlobalAddSwarmingType = "Schwarmtrieb Typ hinzufügen",
      msgGlobalDeleteSwarmingType = "Schwarmtrieb Typ löschen",
      msgGlobalEditSwarmingType = "Schwarmtrieb Typ bearbeiten",
      msgUserIdent = "Login",
      msgUserPassword = "Passwort",
      msgUserEmail = "Email",
      msgUserIsAdmin = "Ist Admin?",
      msgUserIsResetPassword = "Neues Passwort generieren? (Wird per Email zugesendet)",
      msgConfigCode = "Code",
      msgConfigStringValue = "String-Wert",
      msgConfigIntValue = "Integer-Wert",
      msgConfigDoubleValue = "Double-Wert",
      msgConfigBoolValue = "Boolean-Wert",
      msgRawdataBytes = "Bytes",
      msgLocationName = "Name",
      msgHiveLocationId = "Standort",
      msgHiveName = "Name",
      msgHiveQueenYear = "Königin Jahr",
      msgHiveDescription = "Beschreibung",
      msgHiveIsDissolved = "Ist aufgelöst?",
      msgInspectionHiveId = "",
      msgInspectionDate = "Datum",
      msgInspectionSwarmingTypeId = "Schwarmtrieb",
      msgInspectionQueenSeen = "Kö ges.",
      msgInspectionBeeCoveredFrames = "Bel. Waben",
      msgInspectionBroodFrames = "Brutwaben",
      msgInspectionHoneyFrames = "Honigwaben",
      msgInspectionTreatment = "Behandlung",
      msgInspectionFeeding = "Fütterung",
      msgInspectionNotes = "Notizen",
      msgInspectionfileInspectionId = "",
      msgInspectionfileRawdataId = "",
      msgInspectionfileFilename = "Dateiname",
      msgInspectionfileMimetype = "MIME Type",
      msgInspectionfileSize = "Groesse",
      msgInspectionfileFile = "Datei",
      msgSwarmingTypeName = "Name",
      msgSwarmingTypeSortIndex = "Sortierungs-Index",
      msgUserIdentInputInfo = "",
      msgUserPasswordInputInfo = "",
      msgUserEmailInputInfo = "",
      msgUserIsAdminInputInfo = "",
      msgUserIsResetPasswordInputInfo = "",
      msgConfigCodeInputInfo = "",
      msgConfigStringValueInputInfo = "",
      msgConfigIntValueInputInfo = "",
      msgConfigDoubleValueInputInfo = "",
      msgConfigBoolValueInputInfo = "",
      msgRawdataBytesInputInfo = "",
      msgLocationNameInputInfo = "",
      msgHiveLocationIdInputInfo = "",
      msgHiveNameInputInfo = "",
      msgHiveQueenYearInputInfo = "",
      msgHiveDescriptionInputInfo = "",
      msgHiveIsDissolvedInputInfo = "",
      msgInspectionHiveIdInputInfo = "",
      msgInspectionDateInputInfo = "",
      msgInspectionSwarmingTypeIdInputInfo = "",
      msgInspectionQueenSeenInputInfo = "",
      msgInspectionBeeCoveredFramesInputInfo = "",
      msgInspectionBroodFramesInputInfo = "",
      msgInspectionHoneyFramesInputInfo = "",
      msgInspectionTreatmentInputInfo = "",
      msgInspectionFeedingInputInfo = "",
      msgInspectionNotesInputInfo = "",
      msgInspectionfileInspectionIdInputInfo = "",
      msgInspectionfileRawdataIdInputInfo = "",
      msgInspectionfileFilenameInputInfo = "",
      msgInspectionfileMimetypeInputInfo = "",
      msgInspectionfileSizeInputInfo = "",
      msgInspectionfileFileInputInfo = "",
      msgSwarmingTypeNameInputInfo = "",
      msgSwarmingTypeSortIndexInputInfo = "",
      msgTestmailEmail = "Email",
      msgTestmailEmailInputInfo = "",
      msgTestmailTestMail = "Test-Mail",
      msgTestmailSendTestMail = "Test-Mail senden..."
    }

translationEn :: Translation
translationEn =
  Translation
    { msgGlobalHome = "Home",
      msgGlobalAdmin = "Admin",
      msgGlobalLogout = "Logout",
      msgGlobalLanguage = "Language",
      msgGlobalMyProfile = "My Profile",
      msgGlobalEditMyProfile = "Edit my profile",
      msgGlobalReallyDelete = "Are you sure to delete?",
      msgGlobalShowMore = "Show More",
      msgGlobalShowLess = "Show Less",
      msgGlobalShowAll = "Show All",
      msgGlobalUsers = "Users",
      msgGlobalAddUser = "Add user",
      msgGlobalEditUser = "Edit user",
      msgGlobalDeleteUser = "Delete user",
      msgGlobalConfigurations = "Configurations",
      msgGlobalEditConfig = "Edit config",
      msgGlobalCancel = "Cancel",
      msgGlobalInfo = "Info",
      msgGlobalLocation = "Location",
      msgGlobalLocations = "Locations",
      msgGlobalAddLocation = "Add location",
      msgGlobalEditLocation = "Edit location",
      msgGlobalDeleteLocation = "Delete location",
      msgGlobalLocationMasterData = "Location data",
      msgGlobalHiveOverview = "Hive Overview",
      msgGlobalHive = "Hive",
      msgGlobalHives = "Hives",
      msgGlobalAddHive = "Add hive",
      msgGlobalDeleteHive = "Delete hive",
      msgGlobalDetailHive = "Hive details",
      msgGlobalEditHive = "Edit hive",
      msgGlobalHiveMasterData = "Hive data",
      msgGlobalInspection = "Inspection",
      msgGlobalInspections = "Inspections",
      msgGlobalAddInspection = "Add inspection",
      msgGlobalDeleteInspection = "Delete inspection",
      msgGlobalEditInspection = "Edit inspection",
      msgGlobalLastInspection = "Last inspection",
      msgGlobalInspectionfiles = "Files",
      msgGlobalAddInspectionfile = "Add inspection-file",
      msgGlobalDeleteInspectionfile = "Delete inspection-file",
      msgGlobalEditInspectionfile = "Edit inspection-file",
      msgGlobalSwarming = "Swarming mood",
      msgGlobalSwarmingTypes = "Swarming types",
      msgGlobalAddSwarmingType = "Add swarming type",
      msgGlobalDeleteSwarmingType = "Delete swarming type",
      msgGlobalEditSwarmingType = "Edit swarming type",
      msgUserIdent = "Login",
      msgUserPassword = "Password",
      msgUserEmail = "Email",
      msgUserIsAdmin = "Is admin?",
      msgUserIsResetPassword = "Generate new password? (Will be sent by email)",
      msgConfigCode = "Code",
      msgConfigStringValue = "String-Value",
      msgConfigIntValue = "Integer-Value",
      msgConfigDoubleValue = "Double-Value",
      msgConfigBoolValue = "Boolean-Value",
      msgRawdataBytes = "Bytes",
      msgLocationName = "Name",
      msgHiveLocationId = "Location",
      msgHiveName = "Name",
      msgHiveQueenYear = "Queen Year",
      msgHiveDescription = "Description",
      msgHiveIsDissolved = "Is dissolved?",
      msgInspectionHiveId = "",
      msgInspectionDate = "Date",
      msgInspectionSwarmingTypeId = "swarming Mood",
      msgInspectionQueenSeen = "Queen seen",
      msgInspectionBeeCoveredFrames = "Bee covered frames",
      msgInspectionBroodFrames = "Brood frames",
      msgInspectionHoneyFrames = "Honey frames",
      msgInspectionTreatment = "Treatment",
      msgInspectionFeeding = "Feeding",
      msgInspectionNotes = "Notes",
      msgInspectionfileInspectionId = "",
      msgInspectionfileRawdataId = "",
      msgInspectionfileFilename = "Filename",
      msgInspectionfileMimetype = "MIME Type",
      msgInspectionfileSize = "Size",
      msgInspectionfileFile = "File",
      msgSwarmingTypeName = "Name",
      msgSwarmingTypeSortIndex = "Sort Index",
      msgUserIdentInputInfo = "",
      msgUserPasswordInputInfo = "",
      msgUserEmailInputInfo = "",
      msgUserIsAdminInputInfo = "",
      msgUserIsResetPasswordInputInfo = "",
      msgConfigCodeInputInfo = "",
      msgConfigStringValueInputInfo = "",
      msgConfigIntValueInputInfo = "",
      msgConfigDoubleValueInputInfo = "",
      msgConfigBoolValueInputInfo = "",
      msgRawdataBytesInputInfo = "",
      msgLocationNameInputInfo = "",
      msgHiveLocationIdInputInfo = "",
      msgHiveNameInputInfo = "",
      msgHiveQueenYearInputInfo = "",
      msgHiveDescriptionInputInfo = "",
      msgHiveIsDissolvedInputInfo = "",
      msgInspectionHiveIdInputInfo = "",
      msgInspectionDateInputInfo = "",
      msgInspectionSwarmingTypeIdInputInfo = "",
      msgInspectionQueenSeenInputInfo = "",
      msgInspectionBeeCoveredFramesInputInfo = "",
      msgInspectionBroodFramesInputInfo = "",
      msgInspectionHoneyFramesInputInfo = "",
      msgInspectionTreatmentInputInfo = "",
      msgInspectionFeedingInputInfo = "",
      msgInspectionNotesInputInfo = "",
      msgInspectionfileInspectionIdInputInfo = "",
      msgInspectionfileRawdataIdInputInfo = "",
      msgInspectionfileFilenameInputInfo = "",
      msgInspectionfileMimetypeInputInfo = "",
      msgInspectionfileSizeInputInfo = "",
      msgInspectionfileFileInputInfo = "",
      msgSwarmingTypeNameInputInfo = "",
      msgSwarmingTypeSortIndexInputInfo = "",
      msgTestmailEmail = "Email",
      msgTestmailEmailInputInfo = "",
      msgTestmailTestMail = "Test-Mail",
      msgTestmailSendTestMail = "Send Test-Mail..."
    }
-- gen i18n - end
