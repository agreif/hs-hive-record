{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric         #-}

module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import qualified Crypto.PasswordStore as Crypto
import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Random
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import Text.Printf
import qualified Data.Maybe as M
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as B
import Data.Time

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


data VPostSubmitSuccess = VPostSubmitSuccess
  { fsPostSuccessDataJsonUrl :: Text
  }
instance ToJSON VPostSubmitSuccess where
  toJSON o = object
    [ "dataJsonUrl" .= fsPostSuccessDataJsonUrl o
    ]

data VFormSubmitSuccess = VFormSubmitSuccess
  { fsSuccessDataJsonUrl :: Text
  }
instance ToJSON VFormSubmitSuccess where
  toJSON o = object
    [ "isFormValid" .= True
    , "dataJsonUrl" .= fsSuccessDataJsonUrl o
    ]

data VFormSubmitInvalid = VFormSubmitInvalid
  { fsInvalidModalWidgetHtml :: Text
  }
instance ToJSON VFormSubmitInvalid where
  toJSON o = object
    [ "modalWidgetHtml" .= fsInvalidModalWidgetHtml o
    ]

data VFormSubmitStale = VFormSubmitStale
  { fsStaleDataJsonUrl :: Text
  }
instance ToJSON VFormSubmitStale where
  toJSON o = object
    [ "isStaleObjectState" .= True
    , "dataJsonUrl" .= fsStaleDataJsonUrl o
    ]


data JData = JData
  { jDataAppName :: Text
  , jDataUserIdent :: Text
  , jDataMainNavItems :: [JDataNavItem]
  , jDataSubNavItems :: [JDataNavItem]
  , jDataPages :: JDataPages
  , jDataHistoryState :: Maybe JDataHistoryState
  , jDataCsrfToken :: Maybe Text
  , jDataCsrfHeaderName :: Text
  , jDataBreadcrumbItems :: [JDataBreadcrumbItem]
  , jDataCurrentLanguage :: Language
  , jDataTranslation :: Translation
  , jDataLanguageDeUrl :: Text
  , jDataLanguageEnUrl :: Text
  }
instance ToJSON JData where
  toJSON o = object
    [ "appName" .= jDataAppName o
    , "userIdent" .= jDataUserIdent o
    , "mainNavItems" .= jDataMainNavItems o
    , "subNavItems" .= jDataSubNavItems o
    , "pages" .= jDataPages o
    , "historyState" .= jDataHistoryState o
    , "csrfHeaderName" .= jDataCsrfHeaderName o
    , "csrfToken" .= jDataCsrfToken o
    , "breadcrumbItems" .= jDataBreadcrumbItems o
    , "currentLanguage" .= jDataCurrentLanguage o
    , "translation" .= jDataTranslation o
    , "languageDeUrl" .= jDataLanguageDeUrl o
    , "languageEnUrl" .= jDataLanguageEnUrl o
    ]

data JDataNavItem = JDataNavItem
  { jDataNavItemLabel :: Text
  , jDataNavItemIsActive :: Bool
  , jDataNavItemPageDataUrl :: Text
  , jDataNavItemBadge :: Maybe Text
  }
instance ToJSON JDataNavItem where
  toJSON o = object
    [ "label" .= jDataNavItemLabel o
    , "isActive" .= jDataNavItemIsActive o
    , "dataUrl" .= jDataNavItemPageDataUrl o
    , "badge" .= jDataNavItemBadge o
    ]


data JDataBreadcrumbItem = JDataBreadcrumbItem
  { jDataBreadcrumbItemLabel :: Text
  , jDataBreadcrumbItemDataUrl :: Text
  }
instance ToJSON JDataBreadcrumbItem where
  toJSON o = object
    [ "label" .= jDataBreadcrumbItemLabel o
    , "dataUrl" .= jDataBreadcrumbItemDataUrl o
    ]


data JDataHistoryState = JDataHistoryState
  { jDataHistoryStateUrl :: Text
  , jDataHistoryStateTitle :: Text
  }
instance ToJSON JDataHistoryState where
  toJSON o = object
    [ "url" .= jDataHistoryStateUrl o
    , "title" .= jDataHistoryStateTitle o
    ]

instance ToJSON User where
  toJSON o = object
    [ "ident" .= userIdent o
    , "email" .= userEmail o
    , "isAdmin" .= userIsAdmin o
    ]

data JDataPages = JDataPages
  { jDataPageHome :: Maybe JDataPageHome
  , jDataPageAdmin :: Maybe JDataPageAdmin
  , jDataPageLocationList :: Maybe JDataPageLocationList
  , jDataPageLocationDetail :: Maybe JDataPageLocationDetail
  , jDataPageHiveDetail :: Maybe JDataPageHiveDetail
  }
instance ToJSON JDataPages where
  toJSON o = object
    [ "home" .= jDataPageHome o
    , "admin" .= jDataPageAdmin o
    , "locationList" .= jDataPageLocationList o
    , "locationDetail" .= jDataPageLocationDetail o
    , "hiveDetail" .= jDataPageHiveDetail o
    ]

defaultDataPages :: JDataPages
defaultDataPages = JDataPages
  { jDataPageHome = Nothing
  , jDataPageAdmin = Nothing
  , jDataPageLocationList = Nothing
  , jDataPageLocationDetail = Nothing
  , jDataPageHiveDetail = Nothing
  }


data JDataPageHome = JDataPageHome
  { jDataPageHomeContent :: Text
  }
instance ToJSON JDataPageHome where
  toJSON o = object
    [ "content" .= jDataPageHomeContent o
    ]


data JDataPageAdmin = JDataPageAdmin
  { jDataPageAdminUsers :: [JDataUser]
  , jDataPageAdminConfigs :: [JDataConfig]
  , jDataPageAdminTemperTypes :: [JDataTemperType]
  , jDataPageAdminRunningTypes :: [JDataRunningType]
  }
instance ToJSON JDataPageAdmin where
  toJSON o = object
    [ "users" .= jDataPageAdminUsers o
    , "configs" .= jDataPageAdminConfigs o
    , "temperTypes" .= jDataPageAdminTemperTypes o
    , "runningTypes" .= jDataPageAdminRunningTypes o
    ]


data JDataUser = JDataUser
  { jDataUserEnt :: Entity User
  , jDataUserEditFormUrl :: Text
  , jDataUserDeleteFormUrl :: Text
  }
instance ToJSON JDataUser where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataUserEnt o)
    , "editFormUrl" .= jDataUserEditFormUrl o
    , "deleteFormUrl" .= jDataUserDeleteFormUrl o
    ]


data JDataConfig = JDataConfig
  { jDataConfigEnt :: Entity Config
  , jDataConfigEditFormUrl :: Text
  }
instance ToJSON JDataConfig where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataConfigEnt o)
    , "editFormUrl" .= jDataConfigEditFormUrl o
    ]



data JDataPageLocationList = JDataPageLocationList
  { jDataPageLocationListLocations :: [JDataLocation]
  }
instance ToJSON JDataPageLocationList where
  toJSON o = object
    [ "locations" .= jDataPageLocationListLocations o
    ]
data JDataLocation = JDataLocation
  { jDataLocationEnt :: Entity Location
  , jDataLocationDetailPageUrl :: Text
  , jDataLocationDeleteFormUrl :: Text
  }
instance ToJSON JDataLocation where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataLocationEnt o)
    , "detailPageUrl" .= jDataLocationDetailPageUrl o
    , "deleteFormUrl" .= jDataLocationDeleteFormUrl o
    ]

data JDataPageLocationDetail = JDataPageLocationDetail
  { jDataPageLocationDetailLocationEnt :: Entity Location
  , jDataPageLocationDetailLocationEditFormUrl :: Text
  , jDataPageCustomerDetailHives :: [JDataHive]
  , jDataPageCustomerDetailHiveAddFormUrl :: Text
  }
instance ToJSON JDataPageLocationDetail where
  toJSON o = object
    [ "locationEnt" .= jDataPageLocationDetailLocationEnt o
    , "locationEditFormUrl" .= jDataPageLocationDetailLocationEditFormUrl o
    , "hives" .= jDataPageCustomerDetailHives o
    , "hiveAddFormUrl" .= jDataPageCustomerDetailHiveAddFormUrl o
    ]

data JDataHive = JDataHive
  { jDataHiveEnt :: Entity Hive
  , jDataHiveDetailPageUrl :: Text
  , jDataHiveDeleteFormUrl :: Text
  }
instance ToJSON JDataHive where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataHiveEnt o)
    , "detailPageUrl" .= jDataHiveDetailPageUrl o
    , "deleteFormUrl" .= jDataHiveDeleteFormUrl o
    ]


data JDataPageHiveDetail = JDataPageHiveDetail
  { jDataPageHiveDetailHiveEnt :: Entity Hive
  , jDataPageHiveDetailHiveEditFormUrl :: Text
  , jDataPageHiveDetailInspections :: [JDataInspection]
  , jDataPageHiveDetailInspectionAddFormUrl :: Text
  }
instance ToJSON JDataPageHiveDetail where
  toJSON o = object
    [ "hiveEnt" .= jDataPageHiveDetailHiveEnt o
    , "hiveEditFormUrl" .= jDataPageHiveDetailHiveEditFormUrl o
    , "inspections" .= jDataPageHiveDetailInspections o
    , "inspectionAddFormUrl" .= jDataPageHiveDetailInspectionAddFormUrl o
    ]

data JDataInspection = JDataInspection
  { jDataInspectionEnt :: Entity Inspection
  , jDataInspectionEditFormUrl :: Text
  , jDataInspectionDeleteFormUrl :: Text
  }
instance ToJSON JDataInspection where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataInspectionEnt o)
    , "editFormUrl" .= jDataInspectionEditFormUrl o
    , "deleteFormUrl" .= jDataInspectionDeleteFormUrl o
    ]


data JDataTemperType = JDataTemperType
  { jDataTemperTypeEnt :: Entity TemperType
  , jDataTemperTypeEditFormUrl :: Text
  , jDataTemperTypeDeleteFormUrl :: Text
  }
instance ToJSON JDataTemperType where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataTemperTypeEnt o)
    , "editFormUrl" .= jDataTemperTypeEditFormUrl o
    , "deleteFormUrl" .= jDataTemperTypeDeleteFormUrl o
    ]


data JDataRunningType = JDataRunningType
  { jDataRunningTypeEnt :: Entity RunningType
  , jDataRunningTypeEditFormUrl :: Text
  , jDataRunningTypeDeleteFormUrl :: Text
  }
instance ToJSON JDataRunningType where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataRunningTypeEnt o)
    , "editFormUrl" .= jDataRunningTypeEditFormUrl o
    , "deleteFormUrl" .= jDataRunningTypeDeleteFormUrl o
    ]




--------------------------------------------------------------------------------
-- navigation helpers
--------------------------------------------------------------------------------

data MainNav
  = MainNavHome
  | MainNavAdmin
  | MainNavLocation
  deriving (Eq)

mainNavData :: User -> MainNav -> Handler [JDataNavItem]
mainNavData user mainNav = do
  urlRenderer <- getUrlRender
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  msgLocations <- localizedMsg MsgGlobalLocations
  return $
    [ JDataNavItem
      { jDataNavItemLabel = msgHome
      , jDataNavItemIsActive = mainNav == MainNavHome
      , jDataNavItemPageDataUrl = urlRenderer $ HiverecR HomePageDataJsonR
      , jDataNavItemBadge = Nothing
      }
    ]
    ++
    case userIsAdmin user of
      True -> [ JDataNavItem
                { jDataNavItemLabel = msgAdmin
                , jDataNavItemIsActive = mainNav == MainNavAdmin
                , jDataNavItemPageDataUrl = urlRenderer $ AdminR AdminPageDataJsonR
                , jDataNavItemBadge = Nothing
                } ]
      False -> []
    ++
    [ JDataNavItem
      { jDataNavItemLabel = msgLocations
      , jDataNavItemIsActive = mainNav == MainNavLocation
      , jDataNavItemPageDataUrl = urlRenderer $ HiverecR LocationListPageDataJsonR
      , jDataNavItemBadge = Nothing
      }
    ]
--------------------------------------------------------------------------------
-- generic helpers
--------------------------------------------------------------------------------

rnd :: (RandomGen g) => Int -> Int -> Rand g Int
rnd x y = getRandomR (x, y)

randomMixedCaseString :: Int -> IO String
randomMixedCaseString len = do
  values <- evalRandIO (sequence (replicate len $ rnd 65 90))
  let str = toLower $ map C.chr values
  -- in average in every 2 chars is an uppercase char
  str' <- upcaseChars (quot len 2) str
  return str'
  where
    upcaseChars :: Int -> String -> IO String
    upcaseChars countChars str =
      if countChars == 0 then
        return str
      else do
        p <- evalRandIO $ rnd 0 (length str - 2)
        let (prefix, c:suffix) = splitAt p str
        upcaseChars (countChars-1) (prefix ++ toUpper [c] ++ suffix)

randomMixedCaseText :: Int -> IO Text
randomMixedCaseText len = do
  str <- randomMixedCaseString len
  return $ pack str

cryptoHashText :: Text -> IO Text
cryptoHashText text = do
  strHash <- Crypto.makePassword (BSC.pack $ unpack text) 17
  return $ decodeUtf8 strHash

generatePassword :: Int -> IO (Text, Text)
generatePassword len = do
  passwdStr <- randomMixedCaseString len
  passwdHash <- cryptoHashText $ pack passwdStr
  return (pack passwdStr, passwdHash)

renderUrlToText :: Route App -> Handler Text
renderUrlToText route = do
  renderUrl <- getUrlRender
  return $ renderUrl route

constTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Text
constTextField myText = Field
    { fieldParse = parseHelper $ \_ -> Right myText
    , fieldView = \theId name attrs _ isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" step=1 :isReq:required="" value="#{myText}">
|]
    , fieldEnctype = UrlEncoded
    }

dbSystemUser :: Text
dbSystemUser = "system"

groupEntities :: [(Entity a, Entity b)] -> [(Entity a, [Entity b])]
groupEntities es =
  L.map ((\(es1, es2) -> (L.head es1, es2)) . L.unzip) $
  L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

groupEntitiesMaybe :: [(Entity a, Maybe (Entity b))] -> [(Entity a, [Entity b])]
groupEntitiesMaybe es =
  L.map ((\(es1, es2) -> (L.head es1, M.catMaybes es2)) . L.unzip) $
  L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

fileBytes :: FileInfo -> Handler B.ByteString
fileBytes fileInfo = do
  bytesL <- runResourceT $ fileSource fileInfo $$ CB.sinkLbs
  return $ toStrict bytesL

humanReadableBytes :: Integer -> String
humanReadableBytes size
  | null pairs = printf "%.0fZiB" (size'/1024^(7::Integer))
  | otherwise  = if unit == "" then printf "%dB" size
                 else printf "%.1f%sB" n unit
  where
    (n, unit):_ = pairs
    pairs = zip (L.iterate (/1024) size') units
    size' = fromIntegral size :: Double
    units = ["","KB","MB","GB","TB","PB","EB","ZB"] :: [String]

getCurrentDay :: IO Day
getCurrentDay = getCurrentTime >>= return . utctDay

--------------------------------------------------------------------------------
-- config helpers
--------------------------------------------------------------------------------

maybeConfigText :: Text -> YesodDB App (Maybe Text)
maybeConfigText code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configStringValue = result})) -> return result
    Nothing -> return Nothing

maybeConfigInt :: Text -> YesodDB App (Maybe Int)
maybeConfigInt code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configIntValue = result})) -> return result
    Nothing -> return Nothing

maybeConfigDouble :: Text -> YesodDB App (Maybe Double)
maybeConfigDouble code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configDoubleValue = result})) -> return result
    Nothing -> return Nothing

configBool :: Text -> YesodDB App Bool
configBool code = do
  maybeConfigEnt <- selectFirst [ConfigCode ==. code] []
  case maybeConfigEnt of
    Just (Entity _ (Config {configBoolValue = result})) -> return result
    Nothing -> return False

configAppName :: YesodDB App Text
configAppName = do
  maybeResult <- maybeConfigText "app_name"
  case maybeResult  of
    Just result -> return result
    Nothing -> return ""

configEmailFrom :: YesodDB App Text
configEmailFrom = do
  maybeResult <- maybeConfigText "email_from"
  case maybeResult  of
    Just result -> return result
    Nothing -> return ""

configMehrwertSteuer :: YesodDB App Double
configMehrwertSteuer = do
  maybeResult <- maybeConfigDouble "mehrwert_steuer"
  case maybeResult  of
    Just result -> return result
    Nothing -> return 0

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just t) = t

--------------------------------------------------------------------------------
-- format helpers
--------------------------------------------------------------------------------

dayFormatGerman :: String
dayFormatGerman = "%d.%m.%Y"

yearMonthFormatGerman :: String
yearMonthFormatGerman = "%m.%Y"

dayFormatHtml5 :: String
dayFormatHtml5 = "%Y-%m-%d"

formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale dayFormatGerman

formatYearMonth :: Day -> Text
formatYearMonth = pack . formatTime defaultTimeLocale yearMonthFormatGerman

formatMaybeDay :: Maybe Day -> Text
formatMaybeDay (Just day) = formatDay day
formatMaybeDay _ = ""

formatLocalTime :: TimeZone -> UTCTime -> String
formatLocalTime timeZone utcTime = formatTime defaultTimeLocale "%d.%m.%Y %H:%M:%S" $ utcToLocalTime timeZone utcTime

formatLocalTimeDayPart :: TimeZone -> UTCTime -> String
formatLocalTimeDayPart timeZone utcTime = formatTime defaultTimeLocale "%d.%m.%Y" $ utcToLocalTime timeZone utcTime

formatDouble :: Double -> Text
formatDouble x = T.replace "." "," (pack $ printf "%.2f" x)

formatMaybeDouble :: Maybe Double -> Text
formatMaybeDouble (Just x) = formatDouble x
formatMaybeDouble _ = ""

formatMaybeInt :: Maybe Int -> Text
formatMaybeInt (Just x) = pack $ show x
formatMaybeInt _ = ""

formatEuro :: Double -> Text
formatEuro x = formatDouble x ++ " €"

formatMaybeEuro :: Maybe Double -> Text
formatMaybeEuro (Just x) = formatEuro x
formatMaybeEuro _ = ""

formatDoublePercent :: Double -> Text
formatDoublePercent x = formatDouble x ++ " %"

formatInt :: Int -> Text
formatInt = pack . show

formatInt4Digits :: Int -> String
formatInt4Digits = printf "%04d"

formatMinuteValue :: Int -> Text
formatMinuteValue minVal = pack $ h1:h2:':':m1:m2
    where (h1:h2:m1:m2) = formatInt4Digits minVal

data Language = DE | EN
  deriving Generic

instance ToJSON Language

lookupSessionLanguage :: Handler (Maybe Language)
lookupSessionLanguage = do
  session <- getSession
  case lookup "_LANG" session of
    Just sessionLanguage -> case sessionLanguage of
                              "en" -> return $ Just EN
                              "en-US" -> return $ Just EN
                              _ -> return $ Just DE
    _ -> return Nothing

getLanguage :: Handler Language
getLanguage = do
  maybeLanguage <- lookupSessionLanguage
  case maybeLanguage of
    Just language -> return language
    _ -> do
      langs <- languages
      return $ case langs of
                 "en":_ -> EN
                 "en-US":_ -> EN
                 _ -> DE

getTranslation :: Handler Translation
getTranslation = do
  lang <- getLanguage
  return $ case lang of
             EN -> translationEn
             DE -> translationDe

-- localizedMsg :: MsgGlobal -> Handler Text
-- localizedMsg message = do
--   master <- getYesod
--   langs <- languages
--   return $ renderMessage master langs message

localizedMsg :: MsgGlobal -> Handler Text
localizedMsg message = do
  master <- getYesod
  language <- getLanguage
  let langs = case language of
                EN -> ["en-US"]
                DE -> ["de"]
  return $ renderMessage master langs message

-- gen i18n global - start
data MsgGlobal =
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
  | MsgGlobalEditHive
  | MsgGlobalHiveMasterData
  | MsgGlobalInspection
  | MsgGlobalInspections
  | MsgGlobalAddInspection
  | MsgGlobalDeleteInspection
  | MsgGlobalEditInspection
  | MsgGlobalTemperTypes
  | MsgGlobalAddTemperType
  | MsgGlobalDeleteTemperType
  | MsgGlobalEditTemperType
  | MsgGlobalRunningTypes
  | MsgGlobalAddRunningType
  | MsgGlobalDeleteRunningType
  | MsgGlobalEditRunningType

instance RenderMessage App MsgGlobal where
  renderMessage _ []        = renderGlobalGerman
  renderMessage _ ("de":_) = renderGlobalGerman
  renderMessage _ ("en":_) = renderGlobalEnglish
  renderMessage _ ("en-US":_) = renderGlobalEnglish
  renderMessage m (_:ls) = renderMessage m ls

renderGlobalGerman :: MsgGlobal -> Text
renderGlobalGerman MsgGlobalHome = "Home"
renderGlobalGerman MsgGlobalAdmin = "Admin"
renderGlobalGerman MsgGlobalLogout = "Logout"
renderGlobalGerman MsgGlobalLanguage = "Sprache"
renderGlobalGerman MsgGlobalMyProfile = "Mein Profil"
renderGlobalGerman MsgGlobalEditMyProfile = "Mein Profil bearbeiten"
renderGlobalGerman MsgGlobalReallyDelete = "Möchten sie wirklich löschen?"
renderGlobalGerman MsgGlobalUsers = "Nutzer"
renderGlobalGerman MsgGlobalAddUser = "Nutzer hinzufügen"
renderGlobalGerman MsgGlobalEditUser = "Nutzer bearbeiten"
renderGlobalGerman MsgGlobalDeleteUser = "Nutzer löschen"
renderGlobalGerman MsgGlobalConfigurations = "Konfigurationen"
renderGlobalGerman MsgGlobalEditConfig = "Konfiguration bearbeiten"
renderGlobalGerman MsgGlobalTestMail = "Test-Mail"
renderGlobalGerman MsgGlobalSendTestMail = "Test-Mail senden..."
renderGlobalGerman MsgGlobalCancel = "Abbrechen"
renderGlobalGerman MsgGlobalLocation = "Standort"
renderGlobalGerman MsgGlobalLocations = "Standorte"
renderGlobalGerman MsgGlobalAddLocation = "Standort hinzufügen"
renderGlobalGerman MsgGlobalEditLocation = "Standort bearbeiten"
renderGlobalGerman MsgGlobalDeleteLocation = "Standort löschen"
renderGlobalGerman MsgGlobalLocationMasterData = "Standort-Daten"
renderGlobalGerman MsgGlobalHive = "Bienenstock"
renderGlobalGerman MsgGlobalHives = "Bienenstöcke"
renderGlobalGerman MsgGlobalAddHive = "Bienenstock hinzufügen"
renderGlobalGerman MsgGlobalDeleteHive = "Bienenstock löschen"
renderGlobalGerman MsgGlobalEditHive = "Bienenstock bearbeiten"
renderGlobalGerman MsgGlobalHiveMasterData = "Stock-Daten"
renderGlobalGerman MsgGlobalInspection = "Durchsicht"
renderGlobalGerman MsgGlobalInspections = "Durchsichten"
renderGlobalGerman MsgGlobalAddInspection = "Durchsicht hinzufügen"
renderGlobalGerman MsgGlobalDeleteInspection = "Durchsicht löschen"
renderGlobalGerman MsgGlobalEditInspection = "Durchsicht bearbeiten"
renderGlobalGerman MsgGlobalTemperTypes = "Sanftmut Typen"
renderGlobalGerman MsgGlobalAddTemperType = "Sanftmut Typ hinzufügen"
renderGlobalGerman MsgGlobalDeleteTemperType = "Sanftmut Typ löschen"
renderGlobalGerman MsgGlobalEditTemperType = "Sanftmut Typ bearbeiten"
renderGlobalGerman MsgGlobalRunningTypes = "Wabensitz Typen"
renderGlobalGerman MsgGlobalAddRunningType = "Wabensitz Typ hinzufügen"
renderGlobalGerman MsgGlobalDeleteRunningType = "Wabensitz Typ löschen"
renderGlobalGerman MsgGlobalEditRunningType = "Wabensitz Typ bearbeiten"

renderGlobalEnglish :: MsgGlobal -> Text
renderGlobalEnglish MsgGlobalHome = "Home"
renderGlobalEnglish MsgGlobalAdmin = "Admin"
renderGlobalEnglish MsgGlobalLogout = "Logout"
renderGlobalEnglish MsgGlobalLanguage = "Language"
renderGlobalEnglish MsgGlobalMyProfile = "My Profile"
renderGlobalEnglish MsgGlobalEditMyProfile = "Edit my profile"
renderGlobalEnglish MsgGlobalReallyDelete = "Are you sure to delete?"
renderGlobalEnglish MsgGlobalUsers = "Users"
renderGlobalEnglish MsgGlobalAddUser = "Add user"
renderGlobalEnglish MsgGlobalEditUser = "Edit user"
renderGlobalEnglish MsgGlobalDeleteUser = "Delete user"
renderGlobalEnglish MsgGlobalConfigurations = "Configurations"
renderGlobalEnglish MsgGlobalEditConfig = "Edit config"
renderGlobalEnglish MsgGlobalTestMail = "Test-Mail"
renderGlobalEnglish MsgGlobalSendTestMail = "Send Test-Mail..."
renderGlobalEnglish MsgGlobalCancel = "Cancel"
renderGlobalEnglish MsgGlobalLocation = "Location"
renderGlobalEnglish MsgGlobalLocations = "Locations"
renderGlobalEnglish MsgGlobalAddLocation = "Add location"
renderGlobalEnglish MsgGlobalEditLocation = "Edit location"
renderGlobalEnglish MsgGlobalDeleteLocation = "Delete location"
renderGlobalEnglish MsgGlobalLocationMasterData = "Location data"
renderGlobalEnglish MsgGlobalHive = "Hive"
renderGlobalEnglish MsgGlobalHives = "Hives"
renderGlobalEnglish MsgGlobalAddHive = "Add hive"
renderGlobalEnglish MsgGlobalDeleteHive = "Delete hive"
renderGlobalEnglish MsgGlobalEditHive = "Edit hive"
renderGlobalEnglish MsgGlobalHiveMasterData = "Hive data"
renderGlobalEnglish MsgGlobalInspection = "Inspection"
renderGlobalEnglish MsgGlobalInspections = "Inspections"
renderGlobalEnglish MsgGlobalAddInspection = "Add inspection"
renderGlobalEnglish MsgGlobalDeleteInspection = "Delete inspection"
renderGlobalEnglish MsgGlobalEditInspection = "Edit inspection"
renderGlobalEnglish MsgGlobalTemperTypes = "Temper types"
renderGlobalEnglish MsgGlobalAddTemperType = "Add temper type"
renderGlobalEnglish MsgGlobalDeleteTemperType = "Delete temper type"
renderGlobalEnglish MsgGlobalEditTemperType = "Edit temper type"
renderGlobalEnglish MsgGlobalRunningTypes = "Running types"
renderGlobalEnglish MsgGlobalAddRunningType = "Add running type"
renderGlobalEnglish MsgGlobalDeleteRunningType = "Delete running type"
renderGlobalEnglish MsgGlobalEditRunningType = "Edit running type"

data Translation = Translation
  { msgGlobalHome :: Maybe Text
  , msgGlobalAdmin :: Maybe Text
  , msgGlobalLogout :: Maybe Text
  , msgGlobalLanguage :: Maybe Text
  , msgGlobalMyProfile :: Maybe Text
  , msgGlobalEditMyProfile :: Maybe Text
  , msgGlobalReallyDelete :: Maybe Text
  , msgGlobalUsers :: Maybe Text
  , msgGlobalAddUser :: Maybe Text
  , msgGlobalEditUser :: Maybe Text
  , msgGlobalDeleteUser :: Maybe Text
  , msgGlobalConfigurations :: Maybe Text
  , msgGlobalEditConfig :: Maybe Text
  , msgGlobalTestMail :: Maybe Text
  , msgGlobalSendTestMail :: Maybe Text
  , msgGlobalCancel :: Maybe Text
  , msgGlobalLocation :: Maybe Text
  , msgGlobalLocations :: Maybe Text
  , msgGlobalAddLocation :: Maybe Text
  , msgGlobalEditLocation :: Maybe Text
  , msgGlobalDeleteLocation :: Maybe Text
  , msgGlobalLocationMasterData :: Maybe Text
  , msgGlobalHive :: Maybe Text
  , msgGlobalHives :: Maybe Text
  , msgGlobalAddHive :: Maybe Text
  , msgGlobalDeleteHive :: Maybe Text
  , msgGlobalEditHive :: Maybe Text
  , msgGlobalHiveMasterData :: Maybe Text
  , msgGlobalInspection :: Maybe Text
  , msgGlobalInspections :: Maybe Text
  , msgGlobalAddInspection :: Maybe Text
  , msgGlobalDeleteInspection :: Maybe Text
  , msgGlobalEditInspection :: Maybe Text
  , msgGlobalTemperTypes :: Maybe Text
  , msgGlobalAddTemperType :: Maybe Text
  , msgGlobalDeleteTemperType :: Maybe Text
  , msgGlobalEditTemperType :: Maybe Text
  , msgGlobalRunningTypes :: Maybe Text
  , msgGlobalAddRunningType :: Maybe Text
  , msgGlobalDeleteRunningType :: Maybe Text
  , msgGlobalEditRunningType :: Maybe Text
  , msgUserIdent :: Maybe Text
  , msgUserPassword :: Maybe Text
  , msgUserEmail :: Maybe Text
  , msgUserIsAdmin :: Maybe Text
  , msgUserIsResetPassword :: Maybe Text
  , msgConfigCode :: Maybe Text
  , msgConfigStringValue :: Maybe Text
  , msgConfigIntValue :: Maybe Text
  , msgConfigDoubleValue :: Maybe Text
  , msgConfigBoolValue :: Maybe Text
  , msgTestmailEmail :: Maybe Text
  , msgLocationName :: Maybe Text
  , msgHiveLocationId :: Maybe Text
  , msgHiveName :: Maybe Text
  , msgHiveDescription :: Maybe Text
  , msgInspectionHiveId :: Maybe Text
  , msgInspectionDate :: Maybe Text
  , msgInspectionNotes :: Maybe Text
  , msgTemperTypeName :: Maybe Text
  , msgTemperTypeSortIndex :: Maybe Text
  , msgRunningTypeName :: Maybe Text
  , msgRunningTypeSortIndex :: Maybe Text
  } deriving Generic

instance ToJSON Translation

translationDe :: Translation
translationDe = Translation
  { msgGlobalHome = Just "Home"
  , msgGlobalAdmin = Just "Admin"
  , msgGlobalLogout = Just "Logout"
  , msgGlobalLanguage = Just "Sprache"
  , msgGlobalMyProfile = Just "Mein Profil"
  , msgGlobalEditMyProfile = Just "Mein Profil bearbeiten"
  , msgGlobalReallyDelete = Just "Möchten sie wirklich löschen?"
  , msgGlobalUsers = Just "Nutzer"
  , msgGlobalAddUser = Just "Nutzer hinzufügen"
  , msgGlobalEditUser = Just "Nutzer bearbeiten"
  , msgGlobalDeleteUser = Just "Nutzer löschen"
  , msgGlobalConfigurations = Just "Konfigurationen"
  , msgGlobalEditConfig = Just "Konfiguration bearbeiten"
  , msgGlobalTestMail = Just "Test-Mail"
  , msgGlobalSendTestMail = Just "Test-Mail senden..."
  , msgGlobalCancel = Just "Abbrechen"
  , msgGlobalLocation = Just "Standort"
  , msgGlobalLocations = Just "Standorte"
  , msgGlobalAddLocation = Just "Standort hinzufügen"
  , msgGlobalEditLocation = Just "Standort bearbeiten"
  , msgGlobalDeleteLocation = Just "Standort löschen"
  , msgGlobalLocationMasterData = Just "Standort-Daten"
  , msgGlobalHive = Just "Bienenstock"
  , msgGlobalHives = Just "Bienenstöcke"
  , msgGlobalAddHive = Just "Bienenstock hinzufügen"
  , msgGlobalDeleteHive = Just "Bienenstock löschen"
  , msgGlobalEditHive = Just "Bienenstock bearbeiten"
  , msgGlobalHiveMasterData = Just "Stock-Daten"
  , msgGlobalInspection = Just "Durchsicht"
  , msgGlobalInspections = Just "Durchsichten"
  , msgGlobalAddInspection = Just "Durchsicht hinzufügen"
  , msgGlobalDeleteInspection = Just "Durchsicht löschen"
  , msgGlobalEditInspection = Just "Durchsicht bearbeiten"
  , msgGlobalTemperTypes = Just "Sanftmut Typen"
  , msgGlobalAddTemperType = Just "Sanftmut Typ hinzufügen"
  , msgGlobalDeleteTemperType = Just "Sanftmut Typ löschen"
  , msgGlobalEditTemperType = Just "Sanftmut Typ bearbeiten"
  , msgGlobalRunningTypes = Just "Wabensitz Typen"
  , msgGlobalAddRunningType = Just "Wabensitz Typ hinzufügen"
  , msgGlobalDeleteRunningType = Just "Wabensitz Typ löschen"
  , msgGlobalEditRunningType = Just "Wabensitz Typ bearbeiten"
  , msgUserIdent = Just "Login"
  , msgUserPassword = Just "Passwort"
  , msgUserEmail = Just "Email"
  , msgUserIsAdmin = Just "Ist Admin?"
  , msgUserIsResetPassword = Just "Neues Passwort generieren? (Wird per Email zugesendet)"
  , msgConfigCode = Just "Code"
  , msgConfigStringValue = Just "String-Wert"
  , msgConfigIntValue = Just "Integer-Wert"
  , msgConfigDoubleValue = Just "Double-Wert"
  , msgConfigBoolValue = Just "Boolean-Wert"
  , msgTestmailEmail = Just "Email"
  , msgLocationName = Just "Name"
  , msgHiveLocationId = Nothing
  , msgHiveName = Just "Name"
  , msgHiveDescription = Just "Beschreibung"
  , msgInspectionHiveId = Nothing
  , msgInspectionDate = Just "Datum"
  , msgInspectionNotes = Just "Notizen"
  , msgTemperTypeName = Just "Name"
  , msgTemperTypeSortIndex = Just "Sortierungs-Index"
  , msgRunningTypeName = Just "Name"
  , msgRunningTypeSortIndex = Just "Sortierungs-Index"
  }

translationEn :: Translation
translationEn = Translation
  { msgGlobalHome = Just "Home"
  , msgGlobalAdmin = Just "Admin"
  , msgGlobalLogout = Just "Logout"
  , msgGlobalLanguage = Just "Language"
  , msgGlobalMyProfile = Just "My Profile"
  , msgGlobalEditMyProfile = Just "Edit my profile"
  , msgGlobalReallyDelete = Just "Are you sure to delete?"
  , msgGlobalUsers = Just "Users"
  , msgGlobalAddUser = Just "Add user"
  , msgGlobalEditUser = Just "Edit user"
  , msgGlobalDeleteUser = Just "Delete user"
  , msgGlobalConfigurations = Just "Configurations"
  , msgGlobalEditConfig = Just "Edit config"
  , msgGlobalTestMail = Just "Test-Mail"
  , msgGlobalSendTestMail = Just "Send Test-Mail..."
  , msgGlobalCancel = Just "Cancel"
  , msgGlobalLocation = Just "Location"
  , msgGlobalLocations = Just "Locations"
  , msgGlobalAddLocation = Just "Add location"
  , msgGlobalEditLocation = Just "Edit location"
  , msgGlobalDeleteLocation = Just "Delete location"
  , msgGlobalLocationMasterData = Just "Location data"
  , msgGlobalHive = Just "Hive"
  , msgGlobalHives = Just "Hives"
  , msgGlobalAddHive = Just "Add hive"
  , msgGlobalDeleteHive = Just "Delete hive"
  , msgGlobalEditHive = Just "Edit hive"
  , msgGlobalHiveMasterData = Just "Hive data"
  , msgGlobalInspection = Just "Inspection"
  , msgGlobalInspections = Just "Inspections"
  , msgGlobalAddInspection = Just "Add inspection"
  , msgGlobalDeleteInspection = Just "Delete inspection"
  , msgGlobalEditInspection = Just "Edit inspection"
  , msgGlobalTemperTypes = Just "Temper types"
  , msgGlobalAddTemperType = Just "Add temper type"
  , msgGlobalDeleteTemperType = Just "Delete temper type"
  , msgGlobalEditTemperType = Just "Edit temper type"
  , msgGlobalRunningTypes = Just "Running types"
  , msgGlobalAddRunningType = Just "Add running type"
  , msgGlobalDeleteRunningType = Just "Delete running type"
  , msgGlobalEditRunningType = Just "Edit running type"
  , msgUserIdent = Just "Login"
  , msgUserPassword = Just "Password"
  , msgUserEmail = Just "Email"
  , msgUserIsAdmin = Just "Is admin?"
  , msgUserIsResetPassword = Just "Generate new password? (Will be sent by email)"
  , msgConfigCode = Just "Code"
  , msgConfigStringValue = Just "String-Value"
  , msgConfigIntValue = Just "Integer-Value"
  , msgConfigDoubleValue = Just "Double-Value"
  , msgConfigBoolValue = Just "Boolean-Value"
  , msgTestmailEmail = Just "Email"
  , msgLocationName = Just "Name"
  , msgHiveLocationId = Nothing
  , msgHiveName = Just "Name"
  , msgHiveDescription = Just "Description"
  , msgInspectionHiveId = Nothing
  , msgInspectionDate = Just "Date"
  , msgInspectionNotes = Just "Notes"
  , msgTemperTypeName = Just "Name"
  , msgTemperTypeSortIndex = Just "Sort Index"
  , msgRunningTypeName = Just "Name"
  , msgRunningTypeSortIndex = Just "Sort Index"
  }

-- gen i18n global - end
