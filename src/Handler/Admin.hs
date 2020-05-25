{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Admin where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Handler.Common
import Import
import Text.Hamlet (hamletFile)

getAdminHomeR :: Handler Html
getAdminHomeR = do
  let route = AdminR AdminPageDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getAdminPageDataR :: Handler Value
getAdminPageDataR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavAdmin
  jDataUsers <- userListJDataEnts
  jDataConfigs <- configListJDataEnts
  jDataSwarmingTypes <- swarmingTypeListJDataEnts
  let pages =
        defaultDataPages
          { jDataPageAdmin =
              Just $
                JDataPageAdmin
                  { jDataPageAdminUsers = jDataUsers,
                    jDataPageAdminConfigs = jDataConfigs,
                    jDataPageAdminSwarmingTypes = jDataSwarmingTypes
                  }
          }
  msgHome <- localizedMsg MsgGlobalHome
  msgAdmin <- localizedMsg MsgGlobalAdmin
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ AdminR AdminPageDataR
  returnJson
    JData
      { jDataAppName = appName,
        jDataUserIdent = userIdent user,
        jDataMainNavItems = mainNavItems,
        jDataSubNavItems = [],
        jDataPages = pages,
        jDataHistoryState =
          Just
            JDataHistoryState
              { jDataHistoryStateUrl = urlRenderer $ AdminR AdminHomeR,
                jDataHistoryStateTitle = msgAdmin
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataR
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgAdmin,
                jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl,
        jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
      }

userListJDataEnts :: Handler [JDataUser]
userListJDataEnts = do
  urlRenderer <- getUrlRender
  userTuples <- runDB loadUserListTuples
  let jUserList =
        map
          ( \userEnt@(Entity userId _) ->
              JDataUser
                { jDataUserEnt = userEnt,
                  jDataUserEditFormUrl = urlRenderer $ AdminR $ EditUserFormR userId,
                  jDataUserDeleteFormUrl = urlRenderer $ AdminR $ DeleteUserFormR userId
                }
          )
          userTuples
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
  let jConfigList =
        map
          ( \configEnt@(Entity configId _) ->
              JDataConfig
                { jDataConfigEnt = configEnt,
                  jDataConfigEditFormUrl = urlRenderer $ AdminR $ EditConfigFormR configId
                }
          )
          configTuples
  return jConfigList

loadConfigListTuples :: YesodDB App [Entity Config]
loadConfigListTuples =
  E.select $ E.from $ \config -> do
    E.orderBy [E.asc (config E.^. ConfigId)]
    return config

swarmingTypeListJDataEnts :: Handler [JDataSwarmingType]
swarmingTypeListJDataEnts = do
  urlRenderer <- getUrlRender
  swarmingTypeTuples <- runDB loadSwarmingTypeListTuples
  let jSwarmingTypeList =
        map
          ( \swarmingTypeEnt@(Entity swarmingTypeId _) ->
              JDataSwarmingType
                { jDataSwarmingTypeEnt = swarmingTypeEnt,
                  jDataSwarmingTypeEditFormUrl = urlRenderer $ AdminR $ EditSwarmingTypeFormR swarmingTypeId,
                  jDataSwarmingTypeDeleteFormUrl = urlRenderer $ AdminR $ DeleteSwarmingTypeFormR swarmingTypeId
                }
          )
          swarmingTypeTuples
  return jSwarmingTypeList

loadSwarmingTypeListTuples :: YesodDB App [Entity SwarmingType]
loadSwarmingTypeListTuples =
  E.select $ E.from $ \swarmingType -> do
    E.orderBy [E.asc (swarmingType E.^. SwarmingTypeSortIndex)]
    return swarmingType
