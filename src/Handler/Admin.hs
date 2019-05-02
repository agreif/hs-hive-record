{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Handler.Common
import Import
import qualified Database.Esqueleto as E
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

getAdminHomeR :: Handler Html
getAdminHomeR = defaultLayout $
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{AdminR $ AdminPageDataJsonR}")
                     \ })
                   |]

getAdminPageDataJsonR :: Handler Value
getAdminPageDataJsonR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavAdmin
  jDataUsers <- userListJDataEnts
  jDataConfigs <- configListJDataEnts
  jDataTemperTypes <- temperTypeListJDataEnts
  jDataRunningTypes <- runningTypeListJDataEnts
  jDataSwarmingTypes <- swarmingTypeListJDataEnts
  let pages =
        defaultDataPages
        { jDataPageAdmin =
            Just $ JDataPageAdmin
            { jDataPageAdminUsers = jDataUsers
            , jDataPageAdminConfigs = jDataConfigs
            , jDataPageAdminTemperTypes = jDataTemperTypes
            , jDataPageAdminRunningTypes = jDataRunningTypes
            , jDataPageAdminSwarmingTypes = jDataSwarmingTypes
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ AdminR AdminHomeR
      , jDataHistoryStateTitle = msgAdmin
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgAdmin
                               , jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl
    , jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
    }

userListJDataEnts :: Handler [JDataUser]
userListJDataEnts = do
  urlRenderer <- getUrlRender
  userTuples <- runDB loadUserListTuples
  let jUserList = map (\userEnt@(Entity userId _) ->
                           JDataUser
                           { jDataUserEnt = userEnt
                           , jDataUserEditFormUrl = urlRenderer $ AdminR $ EditUserFormR userId
                           , jDataUserDeleteFormUrl = urlRenderer $ AdminR $ DeleteUserFormR userId
                           }
                        ) userTuples
  return jUserList

loadUserListTuples :: YesodDB App [Entity User]
loadUserListTuples =
  E.select $ E.from $ \user -> do
    E.orderBy [E.asc (user E.^. UserId)]
    return user

configListJDataEnts :: Handler [JDataConfig]
configListJDataEnts = do
  urlRenderer <- getUrlRender
  configTuples <- runDB loadConfigListTuples
  let jConfigList = map (\configEnt@(Entity configId _) ->
                           JDataConfig
                           { jDataConfigEnt = configEnt
                           , jDataConfigEditFormUrl = urlRenderer $ AdminR $ EditConfigFormR configId
                           }
                        ) configTuples
  return jConfigList

loadConfigListTuples :: YesodDB App [Entity Config]
loadConfigListTuples =
  E.select $ E.from $ \config -> do
    E.orderBy [E.asc (config E.^. ConfigId)]
    return config

temperTypeListJDataEnts :: Handler [JDataTemperType]
temperTypeListJDataEnts = do
  urlRenderer <- getUrlRender
  temperTypeTuples <- runDB loadTemperTypeListTuples
  let jTemperTypeList = map (\temperTypeEnt@(Entity temperTypeId _) ->
                                JDataTemperType
                                { jDataTemperTypeEnt = temperTypeEnt
                                , jDataTemperTypeEditFormUrl = urlRenderer $ AdminR $ EditTemperTypeFormR temperTypeId
                                , jDataTemperTypeDeleteFormUrl = urlRenderer $ AdminR $ DeleteTemperTypeFormR temperTypeId
                                }
                              ) temperTypeTuples
  return jTemperTypeList

loadTemperTypeListTuples :: YesodDB App [Entity TemperType]
loadTemperTypeListTuples =
  E.select $ E.from $ \temperType -> do
    E.orderBy [E.asc (temperType E.^. TemperTypeSortIndex)]
    return temperType

runningTypeListJDataEnts :: Handler [JDataRunningType]
runningTypeListJDataEnts = do
  urlRenderer <- getUrlRender
  runningTypeTuples <- runDB loadRunningTypeListTuples
  let jRunningTypeList = map (\runningTypeEnt@(Entity runningTypeId _) ->
                                JDataRunningType
                                { jDataRunningTypeEnt = runningTypeEnt
                                , jDataRunningTypeEditFormUrl = urlRenderer $ AdminR $ EditRunningTypeFormR runningTypeId
                                , jDataRunningTypeDeleteFormUrl = urlRenderer $ AdminR $ DeleteRunningTypeFormR runningTypeId
                                }
                              ) runningTypeTuples
  return jRunningTypeList

loadRunningTypeListTuples :: YesodDB App [Entity RunningType]
loadRunningTypeListTuples =
  E.select $ E.from $ \runningType -> do
    E.orderBy [E.asc (runningType E.^. RunningTypeSortIndex)]
    return runningType

swarmingTypeListJDataEnts :: Handler [JDataSwarmingType]
swarmingTypeListJDataEnts = do
  urlRenderer <- getUrlRender
  swarmingTypeTuples <- runDB loadSwarmingTypeListTuples
  let jSwarmingTypeList = map (\swarmingTypeEnt@(Entity swarmingTypeId _) ->
                                  JDataSwarmingType
                                  { jDataSwarmingTypeEnt = swarmingTypeEnt
                                  , jDataSwarmingTypeEditFormUrl = urlRenderer $ AdminR $ EditSwarmingTypeFormR swarmingTypeId
                                  , jDataSwarmingTypeDeleteFormUrl = urlRenderer $ AdminR $ DeleteSwarmingTypeFormR swarmingTypeId
                                  }
                              ) swarmingTypeTuples
  return jSwarmingTypeList

loadSwarmingTypeListTuples :: YesodDB App [Entity SwarmingType]
loadSwarmingTypeListTuples =
  E.select $ E.from $ \swarmingType -> do
    E.orderBy [E.asc (swarmingType E.^. SwarmingTypeSortIndex)]
    return swarmingType
