{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Common where

import Control.Monad.Random
import qualified Crypto.PasswordStore as Crypto
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as C
import qualified Data.Conduit.Binary as CB
import Data.FileEmbed (embedFile)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import qualified Database.Esqueleto as E
import Import
import qualified Text.Printf as PF

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/x-icon" $
    toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
  return $ TypedContent typePlain $
    toContent $(embedFile "config/robots.txt")

data VPostSubmitSuccess = VPostSubmitSuccess
  { fsPostSuccessDataJsonUrl :: Text
  }

instance ToJSON VPostSubmitSuccess where
  toJSON o =
    object
      [ "dataJsonUrl" .= fsPostSuccessDataJsonUrl o
      ]

data VFormSubmitSuccess = VFormSubmitSuccess
  { fsSuccessDataJsonUrl :: Text
  }

instance ToJSON VFormSubmitSuccess where
  toJSON o =
    object
      [ "isFormValid" .= True,
        "dataJsonUrl" .= fsSuccessDataJsonUrl o
      ]

data VFormSubmitInvalid = VFormSubmitInvalid
  { fsInvalidModalWidgetHtml :: Text
  }

instance ToJSON VFormSubmitInvalid where
  toJSON o =
    object
      [ "modalWidgetHtml" .= fsInvalidModalWidgetHtml o
      ]

data VFormSubmitStale = VFormSubmitStale
  { fsStaleDataJsonUrl :: Text
  }

instance ToJSON VFormSubmitStale where
  toJSON o =
    object
      [ "isStaleObjectState" .= True,
        "dataJsonUrl" .= fsStaleDataJsonUrl o
      ]

data JData = JData
  { jDataAppName :: Text,
    jDataUserIdent :: Text,
    jDataMainNavItems :: [JDataNavItem],
    jDataSubNavItems :: [JDataNavItem],
    jDataPages :: JDataPages,
    jDataHistoryState :: Maybe JDataHistoryState,
    jDataCsrfToken :: Maybe Text,
    jDataCsrfHeaderName :: Text,
    jDataCsrfParamName :: Text,
    jDataBreadcrumbItems :: [JDataBreadcrumbItem],
    jDataCurrentLanguage :: Language,
    jDataTranslation :: Translation,
    jDataLanguageDeUrl :: Text,
    jDataLanguageEnUrl :: Text
  }

instance ToJSON JData where
  toJSON o =
    object
      [ "appName" .= jDataAppName o,
        "userIdent" .= jDataUserIdent o,
        "mainNavItems" .= jDataMainNavItems o,
        "subNavItems" .= jDataSubNavItems o,
        "pages" .= jDataPages o,
        "historyState" .= jDataHistoryState o,
        "csrfHeaderName" .= jDataCsrfHeaderName o,
        "csrfToken" .= jDataCsrfToken o,
        "breadcrumbItems" .= jDataBreadcrumbItems o,
        "currentLanguage" .= jDataCurrentLanguage o,
        "translation" .= jDataTranslation o,
        "languageDeUrl" .= jDataLanguageDeUrl o,
        "languageEnUrl" .= jDataLanguageEnUrl o
      ]

data JDataNavItem = JDataNavItem
  { jDataNavItemId :: Maybe Text,
    jDataNavItemLabel :: Text,
    jDataNavItemIsActive :: Bool,
    jDataNavItemUrl :: Maybe Text,
    jDataNavItemDataUrl :: Maybe Text,
    jDataNavItemBadge :: Maybe Text,
    jDataNavItemDropdownItems :: Maybe [JDataNavItem]
  }

instance ToJSON JDataNavItem where
  toJSON o =
    object
      [ "id" .= jDataNavItemId o,
        "label" .= jDataNavItemLabel o,
        "isActive" .= jDataNavItemIsActive o,
        "url" .= jDataNavItemUrl o,
        "dataUrl" .= jDataNavItemDataUrl o,
        "badge" .= jDataNavItemBadge o,
        "dropdownItems" .= jDataNavItemDropdownItems o
      ]

data JDataBreadcrumbItem = JDataBreadcrumbItem
  { jDataBreadcrumbItemLabel :: Text,
    jDataBreadcrumbItemDataUrl :: Text
  }

instance ToJSON JDataBreadcrumbItem where
  toJSON o =
    object
      [ "label" .= jDataBreadcrumbItemLabel o,
        "dataUrl" .= jDataBreadcrumbItemDataUrl o
      ]

data JDataHistoryState = JDataHistoryState
  { jDataHistoryStateUrl :: Text,
    jDataHistoryStateTitle :: Text
  }

instance ToJSON JDataHistoryState where
  toJSON o =
    object
      [ "url" .= jDataHistoryStateUrl o,
        "title" .= jDataHistoryStateTitle o
      ]

data JDataPaginationItem = JDataPaginationItem
  { jDataPaginationItemLabel :: Maybe Text,
    jDataPaginationItemDataUrl :: Maybe Text,
    jDataPaginationItemIsActive :: Bool,
    jDataPaginationItemIsDisabled :: Bool,
    jDataPaginationItemIsPrevious :: Bool,
    jDataPaginationItemIsNext :: Bool
  }

instance ToJSON JDataPaginationItem where
  toJSON o =
    object
      [ "label" .= jDataPaginationItemLabel o,
        "dataUrl" .= jDataPaginationItemDataUrl o,
        "isActive" .= jDataPaginationItemIsActive o,
        "isDisabled" .= jDataPaginationItemIsDisabled o,
        "isPrevious" .= jDataPaginationItemIsPrevious o,
        "isNext" .= jDataPaginationItemIsNext o
      ]

instance ToJSON User where
  toJSON o =
    object
      [ "ident" .= userIdent o,
        "email" .= userEmail o,
        "isAdmin" .= userIsAdmin o
      ]

data JDataPages = JDataPages
  { jDataPageHome :: Maybe JDataPageHome,
    jDataPageAdmin :: Maybe JDataPageAdmin,
    jDataPageLocationList :: Maybe JDataPageLocationList,
    jDataPageLocationDetail :: Maybe JDataPageLocationDetail,
    jDataPageHiveOverview :: Maybe JDataPageHiveOverview,
    jDataPageHiveDetail :: Maybe JDataPageHiveDetail
  }

instance ToJSON JDataPages where
  toJSON o =
    object
      [ "home" .= jDataPageHome o,
        "admin" .= jDataPageAdmin o,
        "locationList" .= jDataPageLocationList o,
        "locationDetail" .= jDataPageLocationDetail o,
        "hiveOverview" .= jDataPageHiveOverview o,
        "hiveDetail" .= jDataPageHiveDetail o
      ]

defaultDataPages :: JDataPages
defaultDataPages =
  JDataPages
    { jDataPageHome = Nothing,
      jDataPageAdmin = Nothing,
      jDataPageLocationList = Nothing,
      jDataPageLocationDetail = Nothing,
      jDataPageHiveOverview = Nothing,
      jDataPageHiveDetail = Nothing
    }

data JDataPageHome = JDataPageHome
  { jDataPageHomeContent :: Text
  }

instance ToJSON JDataPageHome where
  toJSON o =
    object
      [ "content" .= jDataPageHomeContent o
      ]

data JDataPageAdmin = JDataPageAdmin
  { jDataPageAdminUsers :: [JDataUser],
    jDataPageAdminConfigs :: [JDataConfig],
    jDataPageAdminSwarmingTypes :: [JDataSwarmingType],
    jDataPageAdminGitCommitDate :: String,
    jDataPageAdminGitCommitMessage :: String,
    jDataPageAdminGitCommitHash :: String,
    jDataPageAdminGitCommitBranch :: String
  }

instance ToJSON JDataPageAdmin where
  toJSON o =
    object
      [ "users" .= jDataPageAdminUsers o,
        "configs" .= jDataPageAdminConfigs o,
        "swarmingTypes" .= jDataPageAdminSwarmingTypes o,
        "gitCommitDate" .= jDataPageAdminGitCommitDate o,
        "gitCommitMessage" .= jDataPageAdminGitCommitMessage o,
        "gitCommitHash" .= jDataPageAdminGitCommitHash o,
        "gitCommitBranch" .= jDataPageAdminGitCommitBranch o
      ]

data JDataUser = JDataUser
  { jDataUserEnt :: Entity User,
    jDataUserEditFormUrl :: Text,
    jDataUserDeleteFormUrl :: Text
  }

instance ToJSON JDataUser where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataUserEnt o),
        "editFormUrl" .= jDataUserEditFormUrl o,
        "deleteFormUrl" .= jDataUserDeleteFormUrl o
      ]

data JDataConfig = JDataConfig
  { jDataConfigEnt :: Entity Config,
    jDataConfigEditFormUrl :: Text
  }

instance ToJSON JDataConfig where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataConfigEnt o),
        "editFormUrl" .= jDataConfigEditFormUrl o
      ]

data JDataPageLocationList = JDataPageLocationList
  { jDataPageLocationListLocations :: [JDataLocation]
  }

instance ToJSON JDataPageLocationList where
  toJSON o =
    object
      [ "locations" .= jDataPageLocationListLocations o
      ]

data JDataLocation = JDataLocation
  { jDataLocationEnt :: Entity Location,
    jDataLocationDetailUrl :: Text,
    jDataLocationDetailDataUrl :: Text,
    jDataLocationDeleteFormUrl :: Text
  }

instance ToJSON JDataLocation where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataLocationEnt o),
        "detailUrl" .= jDataLocationDetailUrl o,
        "detailDataUrl" .= jDataLocationDetailDataUrl o,
        "deleteFormUrl" .= jDataLocationDeleteFormUrl o
      ]

data JDataPageLocationDetail = JDataPageLocationDetail
  { jDataPageLocationDetailLocationEnt :: Entity Location,
    jDataPageLocationDetailLocationEditFormUrl :: Text,
    jDataPageCustomerDetailHives :: [JDataHiveDetail],
    jDataPageCustomerDetailHiveAddFormUrl :: Text
  }

instance ToJSON JDataPageLocationDetail where
  toJSON o =
    object
      [ "locationEnt" .= jDataPageLocationDetailLocationEnt o,
        "locationEditFormUrl" .= jDataPageLocationDetailLocationEditFormUrl o,
        "hives" .= jDataPageCustomerDetailHives o,
        "hiveAddFormUrl" .= jDataPageCustomerDetailHiveAddFormUrl o
      ]

data JDataHiveDetail = JDataHiveDetail
  { jDataHiveDetailHiveEnt :: Entity Hive,
    jDataHiveDetailLastInspectionEnt :: Maybe (Entity Inspection),
    jDataHiveDetailUrl :: Text,
    jDataHiveDetailDataUrl :: Text,
    jDataHiveDeleteFormUrl :: Text,
    jDataHiveQueenColor :: Maybe QueenColor
  }

instance ToJSON JDataHiveDetail where
  toJSON o =
    object
      [ "hiveEnt" .= entityIdToJSON (jDataHiveDetailHiveEnt o),
        "lastInspectionEnt" .= jDataHiveDetailLastInspectionEnt o,
        "detailUrl" .= jDataHiveDetailUrl o,
        "detailDataUrl" .= jDataHiveDetailDataUrl o,
        "deleteFormUrl" .= jDataHiveDeleteFormUrl o,
        "queenColor" .= jDataHiveQueenColor o
      ]

data JDataPageHiveDetail = JDataPageHiveDetail
  { jDataPageHiveDetailHiveEnt :: Entity Hive,
    jDataPageHiveDetailHiveEditFormUrl :: Text,
    jDataPageHiveDetailInspections :: [JDataInspection],
    jDataPageHiveDetailShowLessInspectionsUrl :: Maybe Text,
    jDataPageHiveDetailShowMoreInspectionsUrl :: Maybe Text,
    jDataPageHiveDetailShowAllInspectionsUrl :: Maybe Text,
    jDataPageHiveDetailInspectionAddFormUrl :: Text,
    jDataPageHiveDetailQueenColor :: Maybe QueenColor
  }

instance ToJSON JDataPageHiveDetail where
  toJSON o =
    object
      [ "hiveEnt" .= jDataPageHiveDetailHiveEnt o,
        "hiveEditFormUrl" .= jDataPageHiveDetailHiveEditFormUrl o,
        "inspections" .= jDataPageHiveDetailInspections o,
        "showLessInspectionsUrl" .= jDataPageHiveDetailShowLessInspectionsUrl o,
        "showMoreInspectionsUrl" .= jDataPageHiveDetailShowMoreInspectionsUrl o,
        "showAllInspectionsUrl" .= jDataPageHiveDetailShowAllInspectionsUrl o,
        "inspectionAddFormUrl" .= jDataPageHiveDetailInspectionAddFormUrl o,
        "queenColor" .= jDataPageHiveDetailQueenColor o
      ]

data JDataInspection = JDataInspection
  { jDataInspectionEnt :: Entity Inspection,
    jDataInspectionSwarmingTypeEnt :: Entity SwarmingType,
    jDataInspectionEditFormUrl :: Text,
    jDataInspectionDeleteFormUrl :: Text,
    jDataInspectionInspectionfileAddFormUrl :: Text,
    jDataInspectionInspectionfiles :: [JDataInspectionfile]
  }

instance ToJSON JDataInspection where
  toJSON o =
    object
      [ "inspectionEnt" .= entityIdToJSON (jDataInspectionEnt o),
        "swarmingTypeEnt" .= entityIdToJSON (jDataInspectionSwarmingTypeEnt o),
        "editFormUrl" .= jDataInspectionEditFormUrl o,
        "deleteFormUrl" .= jDataInspectionDeleteFormUrl o,
        "inspectionfileAddFormUrl" .= jDataInspectionInspectionfileAddFormUrl o,
        "inspectionfiles" .= jDataInspectionInspectionfiles o
      ]

data JDataInspectionfile = JDataInspectionfile
  { jDataInspectionfileEnt :: Entity Inspectionfile,
    jDataInspectionfileEditFormUrl :: Text,
    jDataInspectionfileDeleteFormUrl :: Text,
    jDataInspectionfileDownloadUrl :: Text
  }

instance ToJSON JDataInspectionfile where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataInspectionfileEnt o),
        "editFormUrl" .= jDataInspectionfileEditFormUrl o,
        "deleteFormUrl" .= jDataInspectionfileDeleteFormUrl o,
        "downloadUrl" .= jDataInspectionfileDownloadUrl o
      ]

newtype JDataPageHiveOverview = JDataPageHiveOverview
  { jDataPageHiveOverviewHives :: [JDataHiveOverviewHive]
  }

instance ToJSON JDataPageHiveOverview where
  toJSON o =
    object
      [ "hives" .= jDataPageHiveOverviewHives o
      ]

data JDataHiveOverviewHive = JDataHiveOverviewHive
  { jDataHiveOverviewHiveEnt :: Entity Hive,
    jDataHiveOverviewHiveInspections :: [JDataHiveOverviewHiveInspection],
    jDataHiveOverviewInspectionAddFormUrl :: Text,
    jDataHiveOverviewHiveDetailDataUrl :: Text,
    jDataHiveOverviewQueenColor :: Maybe QueenColor
  }

instance ToJSON JDataHiveOverviewHive where
  toJSON o =
    object
      [ "hiveEnt" .= entityIdToJSON (jDataHiveOverviewHiveEnt o),
        "inspections" .= jDataHiveOverviewHiveInspections o,
        "inspectionAddFormUrl" .= jDataHiveOverviewInspectionAddFormUrl o,
        "hiveDetailDataUrl" .= jDataHiveOverviewHiveDetailDataUrl o,
        "queenColor" .= jDataHiveOverviewQueenColor o
      ]

data JDataHiveOverviewHiveInspection = JDataHiveOverviewHiveInspection
  { jDataHiveOverviewHiveInspection :: JDataInspection,
    jDataHiveOverviewHiveInspectionEditFormUrl :: Text
  }

instance ToJSON JDataHiveOverviewHiveInspection where
  toJSON o =
    object
      [ "inspectionEditFormUrl" .= jDataHiveOverviewHiveInspectionEditFormUrl o,
        "inspection" .= jDataHiveOverviewHiveInspection o
      ]

data JDataSwarmingType = JDataSwarmingType
  { jDataSwarmingTypeEnt :: Entity SwarmingType,
    jDataSwarmingTypeEditFormUrl :: Text,
    jDataSwarmingTypeDeleteFormUrl :: Text
  }

instance ToJSON JDataSwarmingType where
  toJSON o =
    object
      [ "entity" .= entityIdToJSON (jDataSwarmingTypeEnt o),
        "editFormUrl" .= jDataSwarmingTypeEditFormUrl o,
        "deleteFormUrl" .= jDataSwarmingTypeDeleteFormUrl o
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
  hivesItemIdent <- newIdent
  return $
    [ JDataNavItem
        { jDataNavItemId = Nothing,
          jDataNavItemLabel = msgHome,
          jDataNavItemIsActive = mainNav == MainNavHome,
          jDataNavItemUrl = Just $ urlRenderer $ HiverecR HiverecHomeR,
          jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR HomePageDataR,
          jDataNavItemBadge = Nothing,
          jDataNavItemDropdownItems = Nothing
        }
    ]
      ++ [ JDataNavItem
             { jDataNavItemId = Nothing,
               jDataNavItemLabel = msgAdmin,
               jDataNavItemIsActive = mainNav == MainNavAdmin,
               jDataNavItemUrl = Just $ urlRenderer $ AdminR AdminHomeR,
               jDataNavItemDataUrl = Just $ urlRenderer $ AdminR AdminPageDataR,
               jDataNavItemBadge = Nothing,
               jDataNavItemDropdownItems = Nothing
             }
           | userIsAdmin user
         ]
      ++ [ JDataNavItem
             { jDataNavItemId = Nothing,
               jDataNavItemLabel = msgLocations,
               jDataNavItemIsActive = mainNav == MainNavLocations,
               jDataNavItemUrl = Just $ urlRenderer $ HiverecR LocationListR,
               jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR LocationListPageDataR,
               jDataNavItemBadge = Nothing,
               jDataNavItemDropdownItems = Nothing
             }
         ]
      ++ [ JDataNavItem
             { jDataNavItemId = Just hivesItemIdent,
               jDataNavItemLabel = msgHives,
               jDataNavItemIsActive = mainNav == MainNavHives,
               jDataNavItemUrl = Nothing,
               jDataNavItemDataUrl = Nothing,
               jDataNavItemBadge = Nothing,
               jDataNavItemDropdownItems = Just hiveNavItems
             }
           | not $ null hiveNavItems
         ]

--------------------------------------------------------------------------------
-- pagination helpers
--------------------------------------------------------------------------------

paginationPagesCount :: Int -> Int -> Int
paginationPagesCount allCount pageSize =
  if mod allCount pageSize == 0 then pageCount' else pageCount' + 1
  where
    pageCount' = div allCount pageSize

getPaginationJDatas :: Int -> Int -> Int -> Int -> (Int -> Route App) -> Handler (Maybe [JDataPaginationItem])
getPaginationJDatas allCount pageSize curPageNum visibleNumsCount' routeFunc = do
  urlRenderer <- getUrlRender
  let visibleNumsCount = if mod visibleNumsCount' 2 == 0 then visibleNumsCount' + 1 else visibleNumsCount'
  let pageCount = paginationPagesCount allCount pageSize
  (firstPageNum, lastPageNum) <-
    if visibleNumsCount >= pageCount
      then return (1, pageCount) -- show all
      else do
        let (firstNum, lastNum) =
              ( curPageNum - div visibleNumsCount 2,
                curPageNum + div visibleNumsCount 2
              )
        let (firstNum', lastNum') = if firstNum < 1 then (1, visibleNumsCount) else (firstNum, lastNum)
        let (firstNum'', lastNum'') =
              if lastNum > pageCount
                then (pageCount - visibleNumsCount + 1, pageCount)
                else (firstNum', lastNum')
        return (firstNum'', lastNum'')
  let pageNums = [firstPageNum .. lastPageNum]
  case pageCount of
    1 -> return Nothing
    _ ->
      return
        $ Just
        $ ( if curPageNum == 1
              then []
              else
                [ JDataPaginationItem
                    { jDataPaginationItemLabel = Nothing,
                      jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc $ curPageNum - 1,
                      jDataPaginationItemIsActive = False,
                      jDataPaginationItemIsDisabled = False,
                      jDataPaginationItemIsPrevious = True,
                      jDataPaginationItemIsNext = False
                    }
                ]
                  ++ ( if firstPageNum == 1
                         then []
                         else
                           [ JDataPaginationItem
                               { jDataPaginationItemLabel = Just "1",
                                 jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc 1,
                                 jDataPaginationItemIsActive = False,
                                 jDataPaginationItemIsDisabled = False,
                                 jDataPaginationItemIsPrevious = False,
                                 jDataPaginationItemIsNext = False
                               }
                           ]
                     )
                  ++ ( if firstPageNum <= 2
                         then []
                         else
                           [ JDataPaginationItem
                               { jDataPaginationItemLabel = Just "...",
                                 jDataPaginationItemDataUrl = Nothing,
                                 jDataPaginationItemIsActive = False,
                                 jDataPaginationItemIsDisabled = True,
                                 jDataPaginationItemIsPrevious = False,
                                 jDataPaginationItemIsNext = False
                               }
                           ]
                     )
          )
          ++ map
            ( \i ->
                JDataPaginationItem
                  { jDataPaginationItemLabel = Just $ formatInt i,
                    jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc $ fromIntegral i,
                    jDataPaginationItemIsActive = i == curPageNum,
                    jDataPaginationItemIsDisabled = False,
                    jDataPaginationItemIsPrevious = False,
                    jDataPaginationItemIsNext = False
                  }
            )
            pageNums
          ++ ( if lastPageNum >= pageCount -1
                 then []
                 else
                   [ JDataPaginationItem
                       { jDataPaginationItemLabel = Just "...",
                         jDataPaginationItemDataUrl = Nothing,
                         jDataPaginationItemIsActive = False,
                         jDataPaginationItemIsDisabled = True,
                         jDataPaginationItemIsPrevious = False,
                         jDataPaginationItemIsNext = False
                       }
                   ]
             )
          ++ ( if lastPageNum == pageCount
                 then []
                 else
                   [ JDataPaginationItem
                       { jDataPaginationItemLabel = Just $ formatInt pageCount,
                         jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc pageCount,
                         jDataPaginationItemIsActive = False,
                         jDataPaginationItemIsDisabled = False,
                         jDataPaginationItemIsPrevious = False,
                         jDataPaginationItemIsNext = False
                       }
                   ]
             )
          ++ if curPageNum == pageCount
            then []
            else
              [ JDataPaginationItem
                  { jDataPaginationItemLabel = Nothing,
                    jDataPaginationItemDataUrl = Just $ urlRenderer $ routeFunc $ curPageNum + 1,
                    jDataPaginationItemIsActive = False,
                    jDataPaginationItemIsDisabled = False,
                    jDataPaginationItemIsPrevious = False,
                    jDataPaginationItemIsNext = True
                  }
              ]

--------------------------------------------------------------------------------
-- app specific helpers
--------------------------------------------------------------------------------

data QueenColor = White | Yellow | Red | Green | Blue
  deriving (Generic)

instance ToJSON QueenColor

calcQueenColor :: Maybe Int -> Maybe QueenColor
calcQueenColor Nothing = Nothing
calcQueenColor (Just year) =
  Just $
    case (year - 2016) `mod` 5 of
      0 -> White
      1 -> Yellow
      2 -> Red
      3 -> Green
      _ -> Blue

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
  tuples <- runDB
    $ E.select
    $ E.from
    $ \(h `E.InnerJoin` l) -> do
      E.on (h E.^. HiveLocationId E.==. l E.^. LocationId)
      E.orderBy [E.asc (l E.^. LocationName), E.asc (h E.^. HiveName)]
      E.where_ $ (h E.^. HiveIsDissolved) E.==. E.val False
      return (h, l)
  urlRenderer <- getUrlRender
  let hiveOverviewItem =
        JDataNavItem
          { jDataNavItemId = Nothing,
            jDataNavItemLabel = msgHiveOverview,
            jDataNavItemIsActive = False,
            jDataNavItemUrl = Just $ urlRenderer $ HiverecR HiveOverviewR,
            jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR HiveOverviewPageDataR,
            jDataNavItemBadge = Nothing,
            jDataNavItemDropdownItems = Nothing
          }
  let hiveItems =
        map
          ( \(Entity hiveId hive, Entity _ location) ->
              JDataNavItem
                { jDataNavItemId = Nothing,
                  jDataNavItemLabel = hiveName hive ++ " (" ++ locationName location ++ ")",
                  jDataNavItemIsActive = False,
                  jDataNavItemUrl = Just $ urlRenderer $ HiverecR $ HiveDetailR hiveId,
                  jDataNavItemDataUrl = Just $ urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId,
                  jDataNavItemBadge = Nothing,
                  jDataNavItemDropdownItems = Nothing
                }
          )
          tuples
  return $ hiveOverviewItem : hiveItems

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
      if countChars == 0
        then return str
        else do
          p <- evalRandIO $ rnd 0 (length str - 2)
          let (prefix, c : suffix) = splitAt p str
          upcaseChars (countChars -1) (prefix ++ toUpper [c] ++ suffix)

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
constTextField myText =
  Field
    { fieldParse = parseHelper $ \_ -> Right myText,
      fieldView = \theId name attrs _ isReq ->
        toWidget
          [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" step=1 :isReq:required="" value="#{myText}">
|],
      fieldEnctype = UrlEncoded
    }

dbSystemUser :: Text
dbSystemUser = "system"

-- groupEntities :: [(Entity a, Entity b)] -> [(Entity a, [Entity b])]
-- groupEntities es =
--   L.map ((\(es1, es2) -> (L.head es1, es2)) . L.unzip) $
--   L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

-- groupEntitiesMaybe :: [(Entity a, Maybe (Entity b))] -> [(Entity a, [Entity b])]
-- groupEntitiesMaybe es =
--   L.map ((\(es1, es2) -> (L.head es1, M.catMaybes es2)) . L.unzip) $
--   L.groupBy (\(Entity id1 _, _) (Entity id2 _, _) -> id1 == id2) es

fileBytes :: FileInfo -> Handler B.ByteString
fileBytes fileInfo = do
  bytesL <- runConduit $ fileSource fileInfo .| CB.sinkLbs
  return $ toStrict bytesL

humanReadableBytes :: Integer -> String
humanReadableBytes size
  | null pairs = PF.printf "%.0fZiB" (size' / 1024 ^ (7 :: Integer))
  | otherwise =
    if unit == ""
      then PF.printf "%dB" size
      else PF.printf "%.1f%sB" n unit
  where
    (n, unit) : _ = pairs
    pairs = zip (L.iterate (/ 1024) size') units
    size' = fromIntegral size :: Double
    units = ["", "KB", "MB", "GB", "TB", "PB", "EB", "ZB"] :: [String]

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
formatDouble x = T.replace "." "," (pack $ PF.printf "%.2f" x)

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
formatInt4Digits = PF.printf "%04d"

formatMinuteValue :: Int -> Text
formatMinuteValue minVal = pack $ h1 : h2 : ':' : m1 : m2
  where
    (h1 : h2 : m1 : m2) = formatInt4Digits minVal

data Language = DE | EN
  deriving (Generic)

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
        "en" : _ -> EN
        "en-US" : _ -> EN
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
