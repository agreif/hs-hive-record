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
  | MsgGlobalConfigurations
  | MsgGlobalEditConfig
  | MsgGlobalCancel
  | MsgGlobalInfo
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
  | MsgNoteDate
  | MsgNoteText
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
  | MsgNoteDateInputInfo
  | MsgNoteTextInputInfo
  | MsgUserUsers
  | MsgUserAddUser
  | MsgUserEditUser
  | MsgUserDeleteUser
  | MsgLocationLocation
  | MsgLocationLocations
  | MsgLocationAddLocation
  | MsgLocationEditLocation
  | MsgLocationDeleteLocation
  | MsgLocationLocationMasterData
  | MsgHiveHiveOverview
  | MsgHiveHive
  | MsgHiveHives
  | MsgHiveAddHive
  | MsgHiveDeleteHive
  | MsgHiveDetailHive
  | MsgHiveEditHive
  | MsgHiveHiveMasterData
  | MsgInspectionInspection
  | MsgInspectionInspections
  | MsgInspectionAddInspection
  | MsgInspectionDeleteInspection
  | MsgInspectionEditInspection
  | MsgInspectionLastInspection
  | MsgInspectionfileInspectionfiles
  | MsgInspectionfileAddInspectionfile
  | MsgInspectionfileDeleteInspectionfile
  | MsgInspectionfileEditInspectionfile
  | MsgSwarmingTypeSwarming
  | MsgSwarmingTypeSwarmingTypes
  | MsgSwarmingTypeAddSwarmingType
  | MsgSwarmingTypeDeleteSwarmingType
  | MsgSwarmingTypeEditSwarmingType
  | MsgNoteNote
  | MsgNoteNotes
  | MsgNoteAddNote
  | MsgNoteEditNote
  | MsgNoteDeleteNote
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
renderMessageGerman MsgGlobalConfigurations = "Konfigurationen"
renderMessageGerman MsgGlobalEditConfig = "Konfiguration bearbeiten"
renderMessageGerman MsgGlobalCancel = "Abbrechen"
renderMessageGerman MsgGlobalInfo = "Info"
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
renderMessageGerman MsgNoteDate = "Datum"
renderMessageGerman MsgNoteText = "Text"
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
renderMessageGerman MsgNoteDateInputInfo = ""
renderMessageGerman MsgNoteTextInputInfo = ""
renderMessageGerman MsgUserUsers = "Nutzer"
renderMessageGerman MsgUserAddUser = "Nutzer hinzufügen"
renderMessageGerman MsgUserEditUser = "Nutzer bearbeiten"
renderMessageGerman MsgUserDeleteUser = "Nutzer löschen"
renderMessageGerman MsgLocationLocation = "Standort"
renderMessageGerman MsgLocationLocations = "Standorte"
renderMessageGerman MsgLocationAddLocation = "Standort hinzufügen"
renderMessageGerman MsgLocationEditLocation = "Standort bearbeiten"
renderMessageGerman MsgLocationDeleteLocation = "Standort löschen"
renderMessageGerman MsgLocationLocationMasterData = "Standort-Daten"
renderMessageGerman MsgHiveHiveOverview = "Bienenstock Übersicht"
renderMessageGerman MsgHiveHive = "Bienenstock"
renderMessageGerman MsgHiveHives = "Bienenstöcke"
renderMessageGerman MsgHiveAddHive = "Bienenstock hinzufügen"
renderMessageGerman MsgHiveDeleteHive = "Bienenstock löschen"
renderMessageGerman MsgHiveDetailHive = "Bienenstock Details"
renderMessageGerman MsgHiveEditHive = "Bienenstock bearbeiten"
renderMessageGerman MsgHiveHiveMasterData = "Stock-Daten"
renderMessageGerman MsgInspectionInspection = "Durchsicht"
renderMessageGerman MsgInspectionInspections = "Durchsichten"
renderMessageGerman MsgInspectionAddInspection = "Durchsicht hinzufügen"
renderMessageGerman MsgInspectionDeleteInspection = "Durchsicht löschen"
renderMessageGerman MsgInspectionEditInspection = "Durchsicht bearbeiten"
renderMessageGerman MsgInspectionLastInspection = "Letzte Durchsicht"
renderMessageGerman MsgInspectionfileInspectionfiles = "Dateien"
renderMessageGerman MsgInspectionfileAddInspectionfile = "Durchsicht-Datei hinzufügen"
renderMessageGerman MsgInspectionfileDeleteInspectionfile = "Durchsicht-Datei löschen"
renderMessageGerman MsgInspectionfileEditInspectionfile = "Durchsicht-Datei bearbeiten"
renderMessageGerman MsgSwarmingTypeSwarming = "Schwarmtrieb"
renderMessageGerman MsgSwarmingTypeSwarmingTypes = "Schwarmtrieb Typen"
renderMessageGerman MsgSwarmingTypeAddSwarmingType = "Schwarmtrieb Typ hinzufügen"
renderMessageGerman MsgSwarmingTypeDeleteSwarmingType = "Schwarmtrieb Typ löschen"
renderMessageGerman MsgSwarmingTypeEditSwarmingType = "Schwarmtrieb Typ bearbeiten"
renderMessageGerman MsgNoteNote = "Notiz"
renderMessageGerman MsgNoteNotes = "Notizen"
renderMessageGerman MsgNoteAddNote = "Notiz hinzufügen"
renderMessageGerman MsgNoteEditNote = "Notiz bearbeiten"
renderMessageGerman MsgNoteDeleteNote = "Notiz löschen"
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
renderMessageEnglish MsgGlobalConfigurations = "Configurations"
renderMessageEnglish MsgGlobalEditConfig = "Edit config"
renderMessageEnglish MsgGlobalCancel = "Cancel"
renderMessageEnglish MsgGlobalInfo = "Info"
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
renderMessageEnglish MsgNoteDate = "Date"
renderMessageEnglish MsgNoteText = "Text"
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
renderMessageEnglish MsgNoteDateInputInfo = ""
renderMessageEnglish MsgNoteTextInputInfo = ""
renderMessageEnglish MsgUserUsers = "Users"
renderMessageEnglish MsgUserAddUser = "Add user"
renderMessageEnglish MsgUserEditUser = "Edit user"
renderMessageEnglish MsgUserDeleteUser = "Delete user"
renderMessageEnglish MsgLocationLocation = "Location"
renderMessageEnglish MsgLocationLocations = "Locations"
renderMessageEnglish MsgLocationAddLocation = "Add location"
renderMessageEnglish MsgLocationEditLocation = "Edit location"
renderMessageEnglish MsgLocationDeleteLocation = "Delete location"
renderMessageEnglish MsgLocationLocationMasterData = "Location data"
renderMessageEnglish MsgHiveHiveOverview = "Hive Overview"
renderMessageEnglish MsgHiveHive = "Hive"
renderMessageEnglish MsgHiveHives = "Hives"
renderMessageEnglish MsgHiveAddHive = "Add hive"
renderMessageEnglish MsgHiveDeleteHive = "Delete hive"
renderMessageEnglish MsgHiveDetailHive = "Hive details"
renderMessageEnglish MsgHiveEditHive = "Edit hive"
renderMessageEnglish MsgHiveHiveMasterData = "Hive data"
renderMessageEnglish MsgInspectionInspection = "Inspection"
renderMessageEnglish MsgInspectionInspections = "Inspections"
renderMessageEnglish MsgInspectionAddInspection = "Add inspection"
renderMessageEnglish MsgInspectionDeleteInspection = "Delete inspection"
renderMessageEnglish MsgInspectionEditInspection = "Edit inspection"
renderMessageEnglish MsgInspectionLastInspection = "Last inspection"
renderMessageEnglish MsgInspectionfileInspectionfiles = "Files"
renderMessageEnglish MsgInspectionfileAddInspectionfile = "Add inspection-file"
renderMessageEnglish MsgInspectionfileDeleteInspectionfile = "Delete inspection-file"
renderMessageEnglish MsgInspectionfileEditInspectionfile = "Edit inspection-file"
renderMessageEnglish MsgSwarmingTypeSwarming = "Swarming mood"
renderMessageEnglish MsgSwarmingTypeSwarmingTypes = "Swarming types"
renderMessageEnglish MsgSwarmingTypeAddSwarmingType = "Add swarming type"
renderMessageEnglish MsgSwarmingTypeDeleteSwarmingType = "Delete swarming type"
renderMessageEnglish MsgSwarmingTypeEditSwarmingType = "Edit swarming type"
renderMessageEnglish MsgNoteNote = "Note"
renderMessageEnglish MsgNoteNotes = "Notes"
renderMessageEnglish MsgNoteAddNote = "Add note"
renderMessageEnglish MsgNoteEditNote = "Edit note"
renderMessageEnglish MsgNoteDeleteNote = "Delete note"
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
    msgGlobalConfigurations :: Text,
    msgGlobalEditConfig :: Text,
    msgGlobalCancel :: Text,
    msgGlobalInfo :: Text,
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
    msgNoteDate :: Text,
    msgNoteText :: Text,
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
    msgNoteDateInputInfo :: Text,
    msgNoteTextInputInfo :: Text,
    msgUserUsers :: Text,
    msgUserAddUser :: Text,
    msgUserEditUser :: Text,
    msgUserDeleteUser :: Text,
    msgLocationLocation :: Text,
    msgLocationLocations :: Text,
    msgLocationAddLocation :: Text,
    msgLocationEditLocation :: Text,
    msgLocationDeleteLocation :: Text,
    msgLocationLocationMasterData :: Text,
    msgHiveHiveOverview :: Text,
    msgHiveHive :: Text,
    msgHiveHives :: Text,
    msgHiveAddHive :: Text,
    msgHiveDeleteHive :: Text,
    msgHiveDetailHive :: Text,
    msgHiveEditHive :: Text,
    msgHiveHiveMasterData :: Text,
    msgInspectionInspection :: Text,
    msgInspectionInspections :: Text,
    msgInspectionAddInspection :: Text,
    msgInspectionDeleteInspection :: Text,
    msgInspectionEditInspection :: Text,
    msgInspectionLastInspection :: Text,
    msgInspectionfileInspectionfiles :: Text,
    msgInspectionfileAddInspectionfile :: Text,
    msgInspectionfileDeleteInspectionfile :: Text,
    msgInspectionfileEditInspectionfile :: Text,
    msgSwarmingTypeSwarming :: Text,
    msgSwarmingTypeSwarmingTypes :: Text,
    msgSwarmingTypeAddSwarmingType :: Text,
    msgSwarmingTypeDeleteSwarmingType :: Text,
    msgSwarmingTypeEditSwarmingType :: Text,
    msgNoteNote :: Text,
    msgNoteNotes :: Text,
    msgNoteAddNote :: Text,
    msgNoteEditNote :: Text,
    msgNoteDeleteNote :: Text,
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
      msgGlobalConfigurations = "Konfigurationen",
      msgGlobalEditConfig = "Konfiguration bearbeiten",
      msgGlobalCancel = "Abbrechen",
      msgGlobalInfo = "Info",
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
      msgNoteDate = "Datum",
      msgNoteText = "Text",
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
      msgNoteDateInputInfo = "",
      msgNoteTextInputInfo = "",
      msgUserUsers = "Nutzer",
      msgUserAddUser = "Nutzer hinzufügen",
      msgUserEditUser = "Nutzer bearbeiten",
      msgUserDeleteUser = "Nutzer löschen",
      msgLocationLocation = "Standort",
      msgLocationLocations = "Standorte",
      msgLocationAddLocation = "Standort hinzufügen",
      msgLocationEditLocation = "Standort bearbeiten",
      msgLocationDeleteLocation = "Standort löschen",
      msgLocationLocationMasterData = "Standort-Daten",
      msgHiveHiveOverview = "Bienenstock Übersicht",
      msgHiveHive = "Bienenstock",
      msgHiveHives = "Bienenstöcke",
      msgHiveAddHive = "Bienenstock hinzufügen",
      msgHiveDeleteHive = "Bienenstock löschen",
      msgHiveDetailHive = "Bienenstock Details",
      msgHiveEditHive = "Bienenstock bearbeiten",
      msgHiveHiveMasterData = "Stock-Daten",
      msgInspectionInspection = "Durchsicht",
      msgInspectionInspections = "Durchsichten",
      msgInspectionAddInspection = "Durchsicht hinzufügen",
      msgInspectionDeleteInspection = "Durchsicht löschen",
      msgInspectionEditInspection = "Durchsicht bearbeiten",
      msgInspectionLastInspection = "Letzte Durchsicht",
      msgInspectionfileInspectionfiles = "Dateien",
      msgInspectionfileAddInspectionfile = "Durchsicht-Datei hinzufügen",
      msgInspectionfileDeleteInspectionfile = "Durchsicht-Datei löschen",
      msgInspectionfileEditInspectionfile = "Durchsicht-Datei bearbeiten",
      msgSwarmingTypeSwarming = "Schwarmtrieb",
      msgSwarmingTypeSwarmingTypes = "Schwarmtrieb Typen",
      msgSwarmingTypeAddSwarmingType = "Schwarmtrieb Typ hinzufügen",
      msgSwarmingTypeDeleteSwarmingType = "Schwarmtrieb Typ löschen",
      msgSwarmingTypeEditSwarmingType = "Schwarmtrieb Typ bearbeiten",
      msgNoteNote = "Notiz",
      msgNoteNotes = "Notizen",
      msgNoteAddNote = "Notiz hinzufügen",
      msgNoteEditNote = "Notiz bearbeiten",
      msgNoteDeleteNote = "Notiz löschen",
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
      msgGlobalConfigurations = "Configurations",
      msgGlobalEditConfig = "Edit config",
      msgGlobalCancel = "Cancel",
      msgGlobalInfo = "Info",
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
      msgNoteDate = "Date",
      msgNoteText = "Text",
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
      msgNoteDateInputInfo = "",
      msgNoteTextInputInfo = "",
      msgUserUsers = "Users",
      msgUserAddUser = "Add user",
      msgUserEditUser = "Edit user",
      msgUserDeleteUser = "Delete user",
      msgLocationLocation = "Location",
      msgLocationLocations = "Locations",
      msgLocationAddLocation = "Add location",
      msgLocationEditLocation = "Edit location",
      msgLocationDeleteLocation = "Delete location",
      msgLocationLocationMasterData = "Location data",
      msgHiveHiveOverview = "Hive Overview",
      msgHiveHive = "Hive",
      msgHiveHives = "Hives",
      msgHiveAddHive = "Add hive",
      msgHiveDeleteHive = "Delete hive",
      msgHiveDetailHive = "Hive details",
      msgHiveEditHive = "Edit hive",
      msgHiveHiveMasterData = "Hive data",
      msgInspectionInspection = "Inspection",
      msgInspectionInspections = "Inspections",
      msgInspectionAddInspection = "Add inspection",
      msgInspectionDeleteInspection = "Delete inspection",
      msgInspectionEditInspection = "Edit inspection",
      msgInspectionLastInspection = "Last inspection",
      msgInspectionfileInspectionfiles = "Files",
      msgInspectionfileAddInspectionfile = "Add inspection-file",
      msgInspectionfileDeleteInspectionfile = "Delete inspection-file",
      msgInspectionfileEditInspectionfile = "Edit inspection-file",
      msgSwarmingTypeSwarming = "Swarming mood",
      msgSwarmingTypeSwarmingTypes = "Swarming types",
      msgSwarmingTypeAddSwarmingType = "Add swarming type",
      msgSwarmingTypeDeleteSwarmingType = "Delete swarming type",
      msgSwarmingTypeEditSwarmingType = "Edit swarming type",
      msgNoteNote = "Note",
      msgNoteNotes = "Notes",
      msgNoteAddNote = "Add note",
      msgNoteEditNote = "Edit note",
      msgNoteDeleteNote = "Delete note",
      msgTestmailEmail = "Email",
      msgTestmailEmailInputInfo = "",
      msgTestmailTestMail = "Test-Mail",
      msgTestmailSendTestMail = "Send Test-Mail..."
    }
-- gen i18n - end
