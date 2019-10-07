{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Location where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

-------------------------------------------------------
-- list
-------------------------------------------------------

getLocationListR :: Handler Html
getLocationListR =
  defaultLayout $
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{HiverecR $ LocationListPageDataJsonR}")
                     \ })
                   |]

getLocationListPageDataJsonR :: Handler Value
getLocationListPageDataJsonR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavLocations
  jDataLocations <- locationListJDatas
  let pages =
        defaultDataPages
        { jDataPageLocationList =
            Just $ JDataPageLocationList {jDataPageLocationListLocations = jDataLocations}
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgLocations <- localizedMsg MsgGlobalLocations
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ HiverecR LocationListPageDataJsonR
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ HiverecR LocationListR
      , jDataHistoryStateTitle = msgLocations
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgLocations
                               , jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl
    , jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
    }

locationListJDatas :: Handler [JDataLocation]
locationListJDatas = do
  urlRenderer <- getUrlRender
  locationTuples <- runDB loadLocationList
  let jLocationList = map (\locationEnt@(Entity locationId _) ->
                           JDataLocation
                           { jDataLocationEnt = locationEnt
                           , jDataLocationDetailUrl = urlRenderer $ HiverecR $ LocationDetailR locationId
                           , jDataLocationDetailDataUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId
                           , jDataLocationDeleteFormUrl = urlRenderer $ HiverecR $ DeleteLocationFormR locationId
                           }
                        ) locationTuples
  return jLocationList

loadLocationList :: YesodDB App [Entity Location]
loadLocationList = selectList ([] :: [Filter Location]) [Asc LocationName]

-------------------------------------------------------
-- detail
-------------------------------------------------------

getLocationDetailR :: LocationId -> Handler Html
getLocationDetailR locationId =
  defaultLayout $
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{HiverecR $ LocationDetailPageDataJsonR locationId}")
                     \ })
                   |]

getLocationDetailPageDataJsonR :: LocationId -> Handler Value
getLocationDetailPageDataJsonR locationId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  mainNavItems <- mainNavData user MainNavLocations
  location <- runDB $ get404 locationId
  urlRenderer <- getUrlRender
  jDataHives <- locationDetailHiveJDatas locationId
  let pages =
        defaultDataPages
        { jDataPageLocationDetail =
            Just $ JDataPageLocationDetail
            { jDataPageLocationDetailLocationEnt = Entity locationId location
            , jDataPageLocationDetailLocationEditFormUrl = urlRenderer $ HiverecR $ EditLocationFormR locationId
            , jDataPageCustomerDetailHives = jDataHives
            , jDataPageCustomerDetailHiveAddFormUrl = urlRenderer $ HiverecR $ AddHiveFormR locationId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgLocations <- localizedMsg MsgGlobalLocations
  msgLocation <- localizedMsg MsgGlobalLocation
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ HiverecR $ LocationDetailR locationId
      , jDataHistoryStateTitle = msgLocation
      }
    , jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName
    , jDataCsrfToken = reqToken req
    , jDataBreadcrumbItems = [ JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgHome
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = msgLocations
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR LocationListPageDataJsonR }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = locationName location
                               , jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl
    , jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
    }

locationDetailHiveJDatas :: LocationId -> Handler [JDataHiveDetail]
locationDetailHiveJDatas locationId = do
  urlRenderer <- getUrlRender
  hiveEnts <- runDB $ selectList [HiveLocationId ==. locationId] [Asc HiveName]
  hiveDetailTuples <- forM hiveEnts $ \hiveEnt@(Entity hiveId _ ) -> do
    maybeLastInspectionEnt <- runDB $ getLastInspectionEnt hiveId
    return (hiveEnt, maybeLastInspectionEnt)
  return $ map
    (\(hiveEnt@(Entity hiveId _), maybeLastInspectionEnt) ->
       JDataHiveDetail
       { jDataHiveDetailHiveEnt = hiveEnt
       , jDataHiveDetailLastInspectionEnt = maybeLastInspectionEnt
       , jDataHiveDetailUrl = urlRenderer $ HiverecR $ HiveDetailR hiveId
       , jDataHiveDetailDataUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId
       , jDataHiveDeleteFormUrl = urlRenderer $ HiverecR $ DeleteHiveFormR hiveId
       })
    hiveDetailTuples

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddLocation = VAddLocation
  { vAddLocationName :: Text
  }
-- gen data add - end

-- gen get add form - start
getAddLocationFormR :: Handler Html
getAddLocationFormR = do
  (formWidget, _) <- generateFormPost $ vAddLocationForm Nothing
  formLayout $
    toWidget [whamlet|
      <h1>_{MsgGlobalAddLocation}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddLocationR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add form - start
postAddLocationR :: Handler Value
postAddLocationR = do
  ((result, formWidget), _) <- runFormPost $ vAddLocationForm Nothing
  case result of
    FormSuccess vAddLocation -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let location = Location
            {
            locationName = vAddLocationName vAddLocation
            , locationVersion = 1
            , locationCreatedAt = curTime
            , locationCreatedBy = userIdent authUser
            , locationUpdatedAt = curTime
            , locationUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert location
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR LocationListPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add form - end

-- gen add form - start
vAddLocationForm :: Maybe Location -> Html -> MForm Handler (FormResult VAddLocation, Widget)
vAddLocationForm maybeLocation extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (locationName <$> maybeLocation)
  let vAddLocationResult = VAddLocation <$> nameResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    |]
  return (vAddLocationResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgLocationName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditLocation = VEditLocation
  { vEditLocationName :: Text
  , vEditLocationVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditLocationFormR :: LocationId -> Handler Html
getEditLocationFormR locationId = do
  location <- runDB $ get404 locationId
  (formWidget, _) <- generateFormPost $ vEditLocationForm (Just location)
  formLayout $
    toWidget [whamlet|
      <h1>_{MsgGlobalEditLocation}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditLocationR locationId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditLocationR :: LocationId -> Handler Value
postEditLocationR locationId = do
  ((result, formWidget), _) <- runFormPost $ vEditLocationForm Nothing
  case result of
    FormSuccess vEditLocation -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            LocationName =. vEditLocationName vEditLocation
            , LocationVersion =. vEditLocationVersion vEditLocation + 1
            , LocationUpdatedAt =. curTime
            , LocationUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ LocationId ==. locationId
                               , LocationVersion ==. vEditLocationVersion vEditLocation
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditLocationForm :: Maybe Location -> Html -> MForm Handler (FormResult VEditLocation, Widget)
vEditLocationForm maybeLocation extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (locationName <$> maybeLocation)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (locationVersion <$> maybeLocation)
  let vEditLocationResult = VEditLocation <$> nameResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    |]
  return (vEditLocationResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgLocationName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }
-- gen edit form - end

-------------------------------------------------------
-- delete
-------------------------------------------------------

-- gen get delete form - start
getDeleteLocationFormR :: LocationId -> Handler Html
getDeleteLocationFormR locationId = do
  (formWidget, _) <- generateFormPost $ vDeleteLocationForm
  formLayout $
    toWidget [whamlet|
      <h1>_{MsgGlobalDeleteLocation}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteLocationR locationId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteLocationR :: LocationId -> Handler Value
postDeleteLocationR locationId = do
  runDB $ delete locationId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR LocationListPageDataJsonR }
-- gen post delete form - end

-- gen delete form - start
vDeleteLocationForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteLocationForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end

