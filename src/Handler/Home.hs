{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Handler.Common
import Import
import Text.Hamlet (hamletFile)

getHomeR :: Handler Html
getHomeR = redirect $ HiverecR HiverecHomeR

getHiverecHomeR :: Handler Html
getHiverecHomeR = do
  let route = HiverecR HomePageDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getHomePageDataR :: Handler Value
getHomePageDataR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavHome
  let pages =
        defaultDataPages
          { jDataPageHome = Just $ JDataPageHome {jDataPageHomeContent = "todo"}
          }
  msgHome <- localizedMsg MsgGlobalHome
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ HiverecR HomePageDataR
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
              { jDataHistoryStateUrl = urlRenderer $ HiverecR HiverecHomeR,
                jDataHistoryStateTitle = msgHome
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl,
        jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
      }

getRiotBodyTagR :: Handler Html
getRiotBodyTagR = withUrlRenderer $(hamletFile "templates/riot/body_tag.hamlet")

getRiotNavTagR :: Handler Html
getRiotNavTagR = withUrlRenderer $(hamletFile "templates/riot/nav_tag.hamlet")

getRiotPaginationTagR :: Handler Html
getRiotPaginationTagR = withUrlRenderer $(hamletFile "templates/riot/pagination_tag.hamlet")

getRiotQueenDotTagR :: Handler Html
getRiotQueenDotTagR = withUrlRenderer $(hamletFile "templates/riot/queen_dot_tag.hamlet")

getRiotHomePageTagR :: Handler Html
getRiotHomePageTagR = withUrlRenderer $(hamletFile "templates/riot/home_page_tag.hamlet")

getRiotAdminPageTagR :: Handler Html
getRiotAdminPageTagR = withUrlRenderer $(hamletFile "templates/riot/admin_page_tag.hamlet")

getRiotLocationListPageTagR :: Handler Html
getRiotLocationListPageTagR = withUrlRenderer $(hamletFile "templates/riot/location_list_page_tag.hamlet")

getRiotLocationDetailPageTagR :: Handler Html
getRiotLocationDetailPageTagR = withUrlRenderer $(hamletFile "templates/riot/location_detail_page_tag.hamlet")

getRiotHiveOverviewPageTagR :: Handler Html
getRiotHiveOverviewPageTagR = withUrlRenderer $(hamletFile "templates/riot/hive_overview_page_tag.hamlet")

getRiotHiveDetailPageTagR :: Handler Html
getRiotHiveDetailPageTagR = withUrlRenderer $(hamletFile "templates/riot/hive_detail_page_tag.hamlet")

postLanguageDeR :: Text -> Handler Value
postLanguageDeR dataUrlStr = do
  setLanguage "de"
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = dataUrlStr}

postLanguageEnR :: Text -> Handler Value
postLanguageEnR dataUrlStr = do
  setLanguage "en-US"
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = dataUrlStr}
