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
import qualified Database.Esqueleto as E

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
  , jDataNavItemUrl :: Maybe Text
  , jDataNavItemPageDataUrl :: Maybe Text
  , jDataNavItemBadge :: Maybe Text
  , jDataNavItemDropdownItems :: Maybe [JDataNavItem]
  }
instance ToJSON JDataNavItem where
  toJSON o = object
    [ "label" .= jDataNavItemLabel o
    , "isActive" .= jDataNavItemIsActive o
    , "url" .= jDataNavItemUrl o
    , "dataUrl" .= jDataNavItemPageDataUrl o
    , "badge" .= jDataNavItemBadge o
    , "dropdownItems" .= jDataNavItemDropdownItems o
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
  , jDataPageAdminSwarmingTypes :: [JDataSwarmingType]
  }
instance ToJSON JDataPageAdmin where
  toJSON o = object
    [ "users" .= jDataPageAdminUsers o
    , "configs" .= jDataPageAdminConfigs o
    , "temperTypes" .= jDataPageAdminTemperTypes o
    , "runningTypes" .= jDataPageAdminRunningTypes o
    , "swarmingTypes" .= jDataPageAdminSwarmingTypes o
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
  , jDataLocationDetailUrl :: Text
  , jDataLocationDetailPageDataUrl :: Text
  , jDataLocationDeleteFormUrl :: Text
  }
instance ToJSON JDataLocation where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataLocationEnt o)
    , "detailUrl" .= jDataLocationDetailUrl o
    , "detailPageDataUrl" .= jDataLocationDetailPageDataUrl o
    , "deleteFormUrl" .= jDataLocationDeleteFormUrl o
    ]

data JDataPageLocationDetail = JDataPageLocationDetail
  { jDataPageLocationDetailLocationEnt :: Entity Location
  , jDataPageLocationDetailLocationEditFormUrl :: Text
  , jDataPageCustomerDetailHives :: [JDataHiveDetail]
  , jDataPageCustomerDetailHiveAddFormUrl :: Text
  }
instance ToJSON JDataPageLocationDetail where
  toJSON o = object
    [ "locationEnt" .= jDataPageLocationDetailLocationEnt o
    , "locationEditFormUrl" .= jDataPageLocationDetailLocationEditFormUrl o
    , "hives" .= jDataPageCustomerDetailHives o
    , "hiveAddFormUrl" .= jDataPageCustomerDetailHiveAddFormUrl o
    ]

data JDataHiveDetail = JDataHiveDetail
  { jDataHiveDetailHiveEnt :: Entity Hive
  , jDataHiveDetailLastInspectionEnt :: Maybe (Entity Inspection)
  , jDataHiveDetailUrl :: Text
  , jDataHiveDetailPageDataUrl :: Text
  , jDataHiveDeleteFormUrl :: Text
  }
instance ToJSON JDataHiveDetail where
  toJSON o = object
    [ "hiveEnt" .= entityIdToJSON (jDataHiveDetailHiveEnt o)
    , "lastInspectionEnt" .= jDataHiveDetailLastInspectionEnt o
    , "detailUrl" .= jDataHiveDetailUrl o
    , "detailPageDataUrl" .= jDataHiveDetailPageDataUrl o
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
  , jDataInspectionTemperTypeEnt :: Entity TemperType
  , jDataInspectionRunningTypeEnt :: Entity RunningType
  , jDataInspectionSwarmingTypeEnt :: Entity SwarmingType
  , jDataInspectionEditFormUrl :: Text
  , jDataInspectionDeleteFormUrl :: Text
  , jDataInspectionInspectionfileAddFormUrl :: Text
  , jDataInspectionInspectionfiles :: [JDataInspectionfile]
  }
instance ToJSON JDataInspection where
  toJSON o = object
    [ "inspectionEnt" .= entityIdToJSON (jDataInspectionEnt o)
    , "temperTypeEnt" .= entityIdToJSON (jDataInspectionTemperTypeEnt o)
    , "runningTypeEnt" .= entityIdToJSON (jDataInspectionRunningTypeEnt o)
    , "swarmingTypeEnt" .= entityIdToJSON (jDataInspectionSwarmingTypeEnt o)
    , "editFormUrl" .= jDataInspectionEditFormUrl o
    , "deleteFormUrl" .= jDataInspectionDeleteFormUrl o
    , "inspectionfileAddFormUrl" .= jDataInspectionInspectionfileAddFormUrl o
    , "inspectionfiles" .= jDataInspectionInspectionfiles o
    ]
data JDataInspectionfile = JDataInspectionfile
  { jDataInspectionfileEnt :: Entity Inspectionfile
  , jDataInspectionfileEditFormUrl :: Text
  , jDataInspectionfileDeleteFormUrl :: Text
  , jDataInspectionfileDownloadUrl :: Text
  }
instance ToJSON JDataInspectionfile where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataInspectionfileEnt o)
    , "editFormUrl" .= jDataInspectionfileEditFormUrl o
    , "deleteFormUrl" .= jDataInspectionfileDeleteFormUrl o
    , "downloadUrl" .= jDataInspectionfileDownloadUrl o
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


data JDataSwarmingType = JDataSwarmingType
  { jDataSwarmingTypeEnt :: Entity SwarmingType
  , jDataSwarmingTypeEditFormUrl :: Text
  , jDataSwarmingTypeDeleteFormUrl :: Text
  }
instance ToJSON JDataSwarmingType where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataSwarmingTypeEnt o)
    , "editFormUrl" .= jDataSwarmingTypeEditFormUrl o
    , "deleteFormUrl" .= jDataSwarmingTypeDeleteFormUrl o
    ]




--------------------------------------------------------------------------------
-- navigation helpers
--------------------------------------------------------------------------------

data MainNav
  = MainNavHome
  | MainNavAdmin
  | MainNavLocations
  | MainNavHives
  deriving (Eq)

mainNavData :: User -> MainNav -> Handler [JDataNavItem]
mainNavData user mainNav = do
  urlRenderer <- getUrlRender
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  msgLocations <- localizedMsg MsgGlobalLocations
  msgHives <- localizedMsg MsgGlobalHives
  hiveNavItems <- getHiveNavItems
  return $
    [ JDataNavItem
      { jDataNavItemLabel = msgHome
      , jDataNavItemIsActive = mainNav == MainNavHome
      , jDataNavItemUrl = Just $ urlRenderer $ HiverecR HiverecHomeR
      , jDataNavItemPageDataUrl = Just $ urlRenderer $ HiverecR HomePageDataJsonR
      , jDataNavItemBadge = Nothing
      , jDataNavItemDropdownItems = Nothing
      }
    ]
    ++
    case userIsAdmin user of
      True -> [ JDataNavItem
                { jDataNavItemLabel = msgAdmin
                , jDataNavItemIsActive = mainNav == MainNavAdmin
                , jDataNavItemUrl = Just $ urlRenderer $ AdminR AdminHomeR
                , jDataNavItemPageDataUrl = Just $ urlRenderer $ AdminR AdminPageDataJsonR
                , jDataNavItemBadge = Nothing
                , jDataNavItemDropdownItems = Nothing
                } ]
      False -> []
    ++
    [ JDataNavItem
      { jDataNavItemLabel = msgLocations
      , jDataNavItemIsActive = mainNav == MainNavLocations
      , jDataNavItemUrl = Just $ urlRenderer $ HiverecR LocationListR
      , jDataNavItemPageDataUrl = Just $ urlRenderer $ HiverecR LocationListPageDataJsonR
      , jDataNavItemBadge = Nothing
      , jDataNavItemDropdownItems = Nothing
      }
    ]
    ++
    case not $ null hiveNavItems of
      True -> [ JDataNavItem
                { jDataNavItemLabel = msgHives
                , jDataNavItemIsActive = mainNav == MainNavHives
                , jDataNavItemUrl = Nothing
                , jDataNavItemPageDataUrl = Nothing
                , jDataNavItemBadge = Nothing
                , jDataNavItemDropdownItems = Just hiveNavItems
                } ]
      False -> []

--------------------------------------------------------------------------------
-- app specific helpers
--------------------------------------------------------------------------------

getLastInspectionEnt :: HiveId -> YesodDB App (Maybe (Entity Inspection))
getLastInspectionEnt hiveId = do
  inspectionEnts <- E.select $ E.from $ \(h `E.InnerJoin` i) -> do
    E.on (h E.^. HiveId E.==. i E.^. InspectionHiveId)
    E.where_ (h E.^. HiveId E.==. E.val hiveId)
    E.orderBy [E.desc (i E.^. InspectionDate)]
    E.limit 1
    return i
  case inspectionEnts of
    [inspectionEnt] -> return $ Just inspectionEnt
    _ -> return Nothing

getHiveNavItems :: Handler [JDataNavItem]
getHiveNavItems = do
  tuples <- runDB $ do
    E.select $ E.from $ \(h `E.InnerJoin` l) -> do
      E.on (h E.^. HiveLocationId E.==. l E.^. LocationId)
      E.orderBy [E.asc (l E.^. LocationName), E.asc (h E.^. HiveName)]
      return (h, l)
  urlRenderer <- getUrlRender
  forM tuples $ \(Entity hiveId hive, Entity _ location) ->
    return $ JDataNavItem
    { jDataNavItemLabel = hiveName hive ++ " (" ++ locationName location ++ ")"
    , jDataNavItemIsActive = False
    , jDataNavItemUrl = Just $ urlRenderer $ HiverecR $ HiveDetailR hiveId
    , jDataNavItemPageDataUrl = Just $ urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId
    , jDataNavItemBadge = Nothing
    , jDataNavItemDropdownItems = Nothing
    }

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
  passwdStr' <- randomMixedCaseString len
  let passwdStr = T.replace "i" "2" $ T.replace "I" "3" $ T.replace "l" "4" $ T.replace "L" "5" $ T.replace "o" "6" $ T.replace "O" "7" $ T.replace "0" "8" $ T.replace "1" "9" $ pack passwdStr'
  passwdHash <- cryptoHashText passwdStr
  return (passwdStr, passwdHash)

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
renderGlobalGerman MsgGlobalDetailHive = "Bienenstock Details"
renderGlobalGerman MsgGlobalEditHive = "Bienenstock bearbeiten"
renderGlobalGerman MsgGlobalHiveMasterData = "Stock-Daten"
renderGlobalGerman MsgGlobalInspection = "Durchsicht"
renderGlobalGerman MsgGlobalInspectionsAll = "Durchsichten (alle)"
renderGlobalGerman MsgGlobalInspectionsLast10 = "Durchsichten (letzten 10)"
renderGlobalGerman MsgGlobalAddInspection = "Durchsicht hinzufügen"
renderGlobalGerman MsgGlobalDeleteInspection = "Durchsicht löschen"
renderGlobalGerman MsgGlobalEditInspection = "Durchsicht bearbeiten"
renderGlobalGerman MsgGlobalLastInspection = "Letzte Durchsicht"
renderGlobalGerman MsgGlobalInspectionfiles = "Dateien"
renderGlobalGerman MsgGlobalAddInspectionfile = "Durchsicht-Datei hinzufügen"
renderGlobalGerman MsgGlobalDeleteInspectionfile = "Durchsicht-Datei löschen"
renderGlobalGerman MsgGlobalEditInspectionfile = "Durchsicht-Datei bearbeiten"
renderGlobalGerman MsgGlobalTemper = "Sanftmut"
renderGlobalGerman MsgGlobalTemperTypes = "Sanftmut Typen"
renderGlobalGerman MsgGlobalAddTemperType = "Sanftmut Typ hinzufügen"
renderGlobalGerman MsgGlobalDeleteTemperType = "Sanftmut Typ löschen"
renderGlobalGerman MsgGlobalEditTemperType = "Sanftmut Typ bearbeiten"
renderGlobalGerman MsgGlobalRunning = "Wabensitz"
renderGlobalGerman MsgGlobalRunningTypes = "Wabensitz Typen"
renderGlobalGerman MsgGlobalAddRunningType = "Wabensitz Typ hinzufügen"
renderGlobalGerman MsgGlobalDeleteRunningType = "Wabensitz Typ löschen"
renderGlobalGerman MsgGlobalEditRunningType = "Wabensitz Typ bearbeiten"
renderGlobalGerman MsgGlobalSwarming = "Schwarmtrieb"
renderGlobalGerman MsgGlobalSwarmingTypes = "Schwarmtrieb Typen"
renderGlobalGerman MsgGlobalAddSwarmingType = "Schwarmtrieb Typ hinzufügen"
renderGlobalGerman MsgGlobalDeleteSwarmingType = "Schwarmtrieb Typ löschen"
renderGlobalGerman MsgGlobalEditSwarmingType = "Schwarmtrieb Typ bearbeiten"

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
renderGlobalEnglish MsgGlobalDetailHive = "Hive details"
renderGlobalEnglish MsgGlobalEditHive = "Edit hive"
renderGlobalEnglish MsgGlobalHiveMasterData = "Hive data"
renderGlobalEnglish MsgGlobalInspection = "Inspection"
renderGlobalEnglish MsgGlobalInspectionsAll = "Inspections (all)"
renderGlobalEnglish MsgGlobalInspectionsLast10 = "Inspections (last 10)"
renderGlobalEnglish MsgGlobalAddInspection = "Add inspection"
renderGlobalEnglish MsgGlobalDeleteInspection = "Delete inspection"
renderGlobalEnglish MsgGlobalEditInspection = "Edit inspection"
renderGlobalEnglish MsgGlobalLastInspection = "Last inspection"
renderGlobalEnglish MsgGlobalInspectionfiles = "Files"
renderGlobalEnglish MsgGlobalAddInspectionfile = "Add inspection-file"
renderGlobalEnglish MsgGlobalDeleteInspectionfile = "Delete inspection-file"
renderGlobalEnglish MsgGlobalEditInspectionfile = "Edit inspection-file"
renderGlobalEnglish MsgGlobalTemper = "Temper"
renderGlobalEnglish MsgGlobalTemperTypes = "Temper types"
renderGlobalEnglish MsgGlobalAddTemperType = "Add temper type"
renderGlobalEnglish MsgGlobalDeleteTemperType = "Delete temper type"
renderGlobalEnglish MsgGlobalEditTemperType = "Edit temper type"
renderGlobalEnglish MsgGlobalRunning = "Running beh."
renderGlobalEnglish MsgGlobalRunningTypes = "Running types"
renderGlobalEnglish MsgGlobalAddRunningType = "Add running type"
renderGlobalEnglish MsgGlobalDeleteRunningType = "Delete running type"
renderGlobalEnglish MsgGlobalEditRunningType = "Edit running type"
renderGlobalEnglish MsgGlobalSwarming = "Swarming mood"
renderGlobalEnglish MsgGlobalSwarmingTypes = "Swarming types"
renderGlobalEnglish MsgGlobalAddSwarmingType = "Add swarming type"
renderGlobalEnglish MsgGlobalDeleteSwarmingType = "Delete swarming type"
renderGlobalEnglish MsgGlobalEditSwarmingType = "Edit swarming type"

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
  , msgGlobalDetailHive :: Maybe Text
  , msgGlobalEditHive :: Maybe Text
  , msgGlobalHiveMasterData :: Maybe Text
  , msgGlobalInspection :: Maybe Text
  , msgGlobalInspectionsAll :: Maybe Text
  , msgGlobalInspectionsLast10 :: Maybe Text
  , msgGlobalAddInspection :: Maybe Text
  , msgGlobalDeleteInspection :: Maybe Text
  , msgGlobalEditInspection :: Maybe Text
  , msgGlobalLastInspection :: Maybe Text
  , msgGlobalInspectionfiles :: Maybe Text
  , msgGlobalAddInspectionfile :: Maybe Text
  , msgGlobalDeleteInspectionfile :: Maybe Text
  , msgGlobalEditInspectionfile :: Maybe Text
  , msgGlobalTemper :: Maybe Text
  , msgGlobalTemperTypes :: Maybe Text
  , msgGlobalAddTemperType :: Maybe Text
  , msgGlobalDeleteTemperType :: Maybe Text
  , msgGlobalEditTemperType :: Maybe Text
  , msgGlobalRunning :: Maybe Text
  , msgGlobalRunningTypes :: Maybe Text
  , msgGlobalAddRunningType :: Maybe Text
  , msgGlobalDeleteRunningType :: Maybe Text
  , msgGlobalEditRunningType :: Maybe Text
  , msgGlobalSwarming :: Maybe Text
  , msgGlobalSwarmingTypes :: Maybe Text
  , msgGlobalAddSwarmingType :: Maybe Text
  , msgGlobalDeleteSwarmingType :: Maybe Text
  , msgGlobalEditSwarmingType :: Maybe Text
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
  , msgRawdataBytes :: Maybe Text
  , msgLocationName :: Maybe Text
  , msgHiveLocationId :: Maybe Text
  , msgHiveName :: Maybe Text
  , msgHiveDescription :: Maybe Text
  , msgInspectionHiveId :: Maybe Text
  , msgInspectionDate :: Maybe Text
  , msgInspectionTemperTypeId :: Maybe Text
  , msgInspectionRunningTypeId :: Maybe Text
  , msgInspectionSwarmingTypeId :: Maybe Text
  , msgInspectionQueenSeen :: Maybe Text
  , msgInspectionTotalFrames :: Maybe Text
  , msgInspectionBeeCoveredFrames :: Maybe Text
  , msgInspectionBroodFrames :: Maybe Text
  , msgInspectionHoneyFrames :: Maybe Text
  , msgInspectionTreatment :: Maybe Text
  , msgInspectionFeeding :: Maybe Text
  , msgInspectionNotes :: Maybe Text
  , msgInspectionfileInspectionId :: Maybe Text
  , msgInspectionfileRawdataId :: Maybe Text
  , msgInspectionfileFilename :: Maybe Text
  , msgInspectionfileMimetype :: Maybe Text
  , msgInspectionfileSize :: Maybe Text
  , msgInspectionfileFile :: Maybe Text
  , msgTemperTypeName :: Maybe Text
  , msgTemperTypeSortIndex :: Maybe Text
  , msgRunningTypeName :: Maybe Text
  , msgRunningTypeSortIndex :: Maybe Text
  , msgSwarmingTypeName :: Maybe Text
  , msgSwarmingTypeSortIndex :: Maybe Text
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
  , msgGlobalDetailHive = Just "Bienenstock Details"
  , msgGlobalEditHive = Just "Bienenstock bearbeiten"
  , msgGlobalHiveMasterData = Just "Stock-Daten"
  , msgGlobalInspection = Just "Durchsicht"
  , msgGlobalInspectionsAll = Just "Durchsichten (alle)"
  , msgGlobalInspectionsLast10 = Just "Durchsichten (letzten 10)"
  , msgGlobalAddInspection = Just "Durchsicht hinzufügen"
  , msgGlobalDeleteInspection = Just "Durchsicht löschen"
  , msgGlobalEditInspection = Just "Durchsicht bearbeiten"
  , msgGlobalLastInspection = Just "Letzte Durchsicht"
  , msgGlobalInspectionfiles = Just "Dateien"
  , msgGlobalAddInspectionfile = Just "Durchsicht-Datei hinzufügen"
  , msgGlobalDeleteInspectionfile = Just "Durchsicht-Datei löschen"
  , msgGlobalEditInspectionfile = Just "Durchsicht-Datei bearbeiten"
  , msgGlobalTemper = Just "Sanftmut"
  , msgGlobalTemperTypes = Just "Sanftmut Typen"
  , msgGlobalAddTemperType = Just "Sanftmut Typ hinzufügen"
  , msgGlobalDeleteTemperType = Just "Sanftmut Typ löschen"
  , msgGlobalEditTemperType = Just "Sanftmut Typ bearbeiten"
  , msgGlobalRunning = Just "Wabensitz"
  , msgGlobalRunningTypes = Just "Wabensitz Typen"
  , msgGlobalAddRunningType = Just "Wabensitz Typ hinzufügen"
  , msgGlobalDeleteRunningType = Just "Wabensitz Typ löschen"
  , msgGlobalEditRunningType = Just "Wabensitz Typ bearbeiten"
  , msgGlobalSwarming = Just "Schwarmtrieb"
  , msgGlobalSwarmingTypes = Just "Schwarmtrieb Typen"
  , msgGlobalAddSwarmingType = Just "Schwarmtrieb Typ hinzufügen"
  , msgGlobalDeleteSwarmingType = Just "Schwarmtrieb Typ löschen"
  , msgGlobalEditSwarmingType = Just "Schwarmtrieb Typ bearbeiten"
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
  , msgRawdataBytes = Just "Bytes"
  , msgLocationName = Just "Name"
  , msgHiveLocationId = Nothing
  , msgHiveName = Just "Name"
  , msgHiveDescription = Just "Beschreibung"
  , msgInspectionHiveId = Nothing
  , msgInspectionDate = Just "Datum"
  , msgInspectionTemperTypeId = Just "Sanftmut"
  , msgInspectionRunningTypeId = Just "Wabensitz"
  , msgInspectionSwarmingTypeId = Just "Schwarmtrieb"
  , msgInspectionQueenSeen = Just "Kö ges."
  , msgInspectionTotalFrames = Just "Ges. Waben"
  , msgInspectionBeeCoveredFrames = Just "Bel. Waben"
  , msgInspectionBroodFrames = Just "Brutwaben"
  , msgInspectionHoneyFrames = Just "Honigwaben"
  , msgInspectionTreatment = Just "Behandlung"
  , msgInspectionFeeding = Just "Fütterung"
  , msgInspectionNotes = Just "Notizen"
  , msgInspectionfileInspectionId = Nothing
  , msgInspectionfileRawdataId = Nothing
  , msgInspectionfileFilename = Just "Dateiname"
  , msgInspectionfileMimetype = Just "MIME Type"
  , msgInspectionfileSize = Just "Groesse"
  , msgInspectionfileFile = Just "Datei"
  , msgTemperTypeName = Just "Name"
  , msgTemperTypeSortIndex = Just "Sortierungs-Index"
  , msgRunningTypeName = Just "Name"
  , msgRunningTypeSortIndex = Just "Sortierungs-Index"
  , msgSwarmingTypeName = Just "Name"
  , msgSwarmingTypeSortIndex = Just "Sortierungs-Index"
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
  , msgGlobalDetailHive = Just "Hive details"
  , msgGlobalEditHive = Just "Edit hive"
  , msgGlobalHiveMasterData = Just "Hive data"
  , msgGlobalInspection = Just "Inspection"
  , msgGlobalInspectionsAll = Just "Inspections (all)"
  , msgGlobalInspectionsLast10 = Just "Inspections (last 10)"
  , msgGlobalAddInspection = Just "Add inspection"
  , msgGlobalDeleteInspection = Just "Delete inspection"
  , msgGlobalEditInspection = Just "Edit inspection"
  , msgGlobalLastInspection = Just "Last inspection"
  , msgGlobalInspectionfiles = Just "Files"
  , msgGlobalAddInspectionfile = Just "Add inspection-file"
  , msgGlobalDeleteInspectionfile = Just "Delete inspection-file"
  , msgGlobalEditInspectionfile = Just "Edit inspection-file"
  , msgGlobalTemper = Just "Temper"
  , msgGlobalTemperTypes = Just "Temper types"
  , msgGlobalAddTemperType = Just "Add temper type"
  , msgGlobalDeleteTemperType = Just "Delete temper type"
  , msgGlobalEditTemperType = Just "Edit temper type"
  , msgGlobalRunning = Just "Running beh."
  , msgGlobalRunningTypes = Just "Running types"
  , msgGlobalAddRunningType = Just "Add running type"
  , msgGlobalDeleteRunningType = Just "Delete running type"
  , msgGlobalEditRunningType = Just "Edit running type"
  , msgGlobalSwarming = Just "Swarming mood"
  , msgGlobalSwarmingTypes = Just "Swarming types"
  , msgGlobalAddSwarmingType = Just "Add swarming type"
  , msgGlobalDeleteSwarmingType = Just "Delete swarming type"
  , msgGlobalEditSwarmingType = Just "Edit swarming type"
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
  , msgRawdataBytes = Just "Bytes"
  , msgLocationName = Just "Name"
  , msgHiveLocationId = Nothing
  , msgHiveName = Just "Name"
  , msgHiveDescription = Just "Description"
  , msgInspectionHiveId = Nothing
  , msgInspectionDate = Just "Date"
  , msgInspectionTemperTypeId = Just "Temper"
  , msgInspectionRunningTypeId = Just "Running Beh."
  , msgInspectionSwarmingTypeId = Just "swarming Mood"
  , msgInspectionQueenSeen = Just "Queen seen"
  , msgInspectionTotalFrames = Just "Total frames"
  , msgInspectionBeeCoveredFrames = Just "Bee covered frames"
  , msgInspectionBroodFrames = Just "Brood frames"
  , msgInspectionHoneyFrames = Just "Honey frames"
  , msgInspectionTreatment = Just "Treatment"
  , msgInspectionFeeding = Just "Feeding"
  , msgInspectionNotes = Just "Notes"
  , msgInspectionfileInspectionId = Nothing
  , msgInspectionfileRawdataId = Nothing
  , msgInspectionfileFilename = Just "Filename"
  , msgInspectionfileMimetype = Just "MIME Type"
  , msgInspectionfileSize = Just "Size"
  , msgInspectionfileFile = Just "File"
  , msgTemperTypeName = Just "Name"
  , msgTemperTypeSortIndex = Just "Sort Index"
  , msgRunningTypeName = Just "Name"
  , msgRunningTypeSortIndex = Just "Sort Index"
  , msgSwarmingTypeName = Just "Name"
  , msgSwarmingTypeSortIndex = Just "Sort Index"
  }

-- gen i18n global - end
