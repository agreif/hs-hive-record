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
  , jDataNavItemDataUrl :: Maybe Text
  , jDataNavItemBadge :: Maybe Text
  , jDataNavItemDropdownItems :: Maybe [JDataNavItem]
  }
instance ToJSON JDataNavItem where
  toJSON o = object
    [ "label" .= jDataNavItemLabel o
    , "isActive" .= jDataNavItemIsActive o
    , "url" .= jDataNavItemUrl o
    , "dataUrl" .= jDataNavItemDataUrl o
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
  , jDataPageHiveOverview :: Maybe JDataPageHiveOverview
  , jDataPageHiveDetail :: Maybe JDataPageHiveDetail
  }
instance ToJSON JDataPages where
  toJSON o = object
    [ "home" .= jDataPageHome o
    , "admin" .= jDataPageAdmin o
    , "locationList" .= jDataPageLocationList o
    , "locationDetail" .= jDataPageLocationDetail o
    , "hiveOverview" .= jDataPageHiveOverview o
    , "hiveDetail" .= jDataPageHiveDetail o
    ]

defaultDataPages :: JDataPages
defaultDataPages = JDataPages
  { jDataPageHome = Nothing
  , jDataPageAdmin = Nothing
  , jDataPageLocationList = Nothing
  , jDataPageLocationDetail = Nothing
  , jDataPageHiveOverview = Nothing
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
  , jDataLocationDetailDataUrl :: Text
  , jDataLocationDeleteFormUrl :: Text
  }
instance ToJSON JDataLocation where
  toJSON o = object
    [ "entity" .= entityIdToJSON (jDataLocationEnt o)
    , "detailUrl" .= jDataLocationDetailUrl o
    , "detailDataUrl" .= jDataLocationDetailDataUrl o
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
  , jDataHiveDetailDataUrl :: Text
  , jDataHiveDeleteFormUrl :: Text
  }
instance ToJSON JDataHiveDetail where
  toJSON o = object
    [ "hiveEnt" .= entityIdToJSON (jDataHiveDetailHiveEnt o)
    , "lastInspectionEnt" .= jDataHiveDetailLastInspectionEnt o
    , "detailUrl" .= jDataHiveDetailUrl o
    , "detailDataUrl" .= jDataHiveDetailDataUrl o
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


newtype JDataPageHiveOverview = JDataPageHiveOverview
  { jDataPageHiveOverviewHives :: [JDataHiveOverviewHive]
  }
instance ToJSON JDataPageHiveOverview where
  toJSON o = object
    [ "hives" .= jDataPageHiveOverviewHives o
    ]

data JDataHiveOverviewHive = JDataHiveOverviewHive
  { jDataHiveOverviewHiveEnt :: Entity Hive
  , jDataHiveOverviewHiveInspections :: [JDataInspection]
  , jDataHiveOverviewInspectionAddFormUrl :: Text
  }
instance ToJSON JDataHiveOverviewHive where
  toJSON o = object
    [ "hiveEnt" .= entityIdToJSON (jDataHiveOverviewHiveEnt o)
    , "inspections" .= jDataHiveOverviewHiveInspections o
    , "inspectionAddFormUrl" .= jDataHiveOverviewInspectionAddFormUrl o
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
      , jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR HomePageDataJsonR
      , jDataNavItemBadge = Nothing
      , jDataNavItemDropdownItems = Nothing
      }
    ]
    ++
    [ JDataNavItem
      { jDataNavItemLabel = msgAdmin
      , jDataNavItemIsActive = mainNav == MainNavAdmin
      , jDataNavItemUrl = Just $ urlRenderer $ AdminR AdminHomeR
      , jDataNavItemDataUrl = Just $ urlRenderer $ AdminR AdminPageDataJsonR
      , jDataNavItemBadge = Nothing
      , jDataNavItemDropdownItems = Nothing
      }
    | userIsAdmin user ]
    ++
    [ JDataNavItem
      { jDataNavItemLabel = msgLocations
      , jDataNavItemIsActive = mainNav == MainNavLocations
      , jDataNavItemUrl = Just $ urlRenderer $ HiverecR LocationListR
      , jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR LocationListPageDataJsonR
      , jDataNavItemBadge = Nothing
      , jDataNavItemDropdownItems = Nothing
      }
    ]
    ++
    [ JDataNavItem
      { jDataNavItemLabel = msgHives
      , jDataNavItemIsActive = mainNav == MainNavHives
      , jDataNavItemUrl = Nothing
      , jDataNavItemDataUrl = Nothing
      , jDataNavItemBadge = Nothing
      , jDataNavItemDropdownItems = Just hiveNavItems
      }
    | not $ null hiveNavItems ]

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
  msgHiveOverview <- localizedMsg MsgGlobalHiveOverview
  tuples <- runDB $
    E.select $ E.from $ \(h `E.InnerJoin` l) -> do
      E.on (h E.^. HiveLocationId E.==. l E.^. LocationId)
      E.orderBy [E.asc (l E.^. LocationName), E.asc (h E.^. HiveName)]
      E.where_ $ (h E.^. HiveIsDissolved) E.!=. E.val True
      return (h, l)
  urlRenderer <- getUrlRender
  let hiveOverviewItem =
        JDataNavItem
        { jDataNavItemLabel = msgHiveOverview
        , jDataNavItemIsActive = False
        , jDataNavItemUrl = Just $ urlRenderer $ HiverecR HiveOverviewR
        , jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR HiveOverviewPageDataJsonR
        , jDataNavItemBadge = Nothing
        , jDataNavItemDropdownItems = Nothing
        }
  let hiveItems =
        map ( \(Entity hiveId hive, Entity _ location) ->
                JDataNavItem
                { jDataNavItemLabel = hiveName hive ++ " (" ++ locationName location ++ ")"
                , jDataNavItemIsActive = False
                , jDataNavItemUrl = Just $ urlRenderer $ HiverecR $ HiveDetailR hiveId
                , jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId
                , jDataNavItemBadge = Nothing
                , jDataNavItemDropdownItems = Nothing
                }
            )
        tuples
  return $ hiveOverviewItem:hiveItems

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
  upcaseChars (quot len 2) str
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
getCurrentDay = utctDay <$> getCurrentTime

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

parseDay :: Text -> Handler Day
parseDay dayText = parseTimeM False defaultTimeLocale dayFormatGerman $ unpack dayText

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

localizedMsg :: AppMessage -> Handler Text
localizedMsg message = do
  master <- getYesod
  language <- getLanguage
  let langs = case language of
                EN -> ["en-US"]
                DE -> ["de"]
  return $ renderMessage master langs message
