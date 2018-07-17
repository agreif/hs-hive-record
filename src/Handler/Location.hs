{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Location where

import Handler.Common
import Import
--import qualified Database.Esqueleto as E
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

-------------------------------------------------------
-- location list page
-------------------------------------------------------

getLocationListR :: Handler Html
getLocationListR = defaultLayout $ do
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
  appName <- runDB $ configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavLocation
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
                           , jDataLocationDetailPageUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId
                           , jDataLocationDeleteFormUrl = urlRenderer $ HiverecR $ DeleteLocationFormR locationId
                           }
                        ) locationTuples
  return jLocationList

loadLocationList :: YesodDB App [Entity Location]
loadLocationList = selectList ([] :: [Filter Location]) [Asc LocationName]

-------------------------------------------------------
-- location detail page
-------------------------------------------------------

getLocationDetailR :: LocationId -> Handler Html
getLocationDetailR locationId = defaultLayout $ do
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
  appName <- runDB $ configAppName
  mainNavItems <- mainNavData user MainNavLocation
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

locationDetailHiveJDatas :: LocationId -> Handler [JDataHive]
locationDetailHiveJDatas locationId = do
  urlRenderer <- getUrlRender
  hiveEnts <- runDB $ selectList [HiveLocationId ==. locationId] [Asc HiveName]
  return $ map
    (\(hiveEnt@(Entity hiveId _)) ->
       JDataHive
       { jDataHiveEnt = hiveEnt
       , jDataHiveDetailPageUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId
       , jDataHiveDeleteFormUrl = urlRenderer $ HiverecR $ DeleteHiveFormR hiveId
       })
    hiveEnts







-------------------------------------------------------
-- add location
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
  formLayout $ do
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
      _ <- runDB $ insert location
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
      { fsLabel = SomeMessage MsgAddLocationName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }

data MsgAddLocation =
  MsgAddLocationName

instance RenderMessage App MsgAddLocation where
  renderMessage _ []        = renderAddLocationGerman
  renderMessage _ ("de":_) = renderAddLocationGerman
  renderMessage _ ("en":_) = renderAddLocationEnglish
  renderMessage _ ("en-US":_) = renderAddLocationEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderAddLocationGerman :: MsgAddLocation -> Text
renderAddLocationGerman MsgAddLocationName = "Name"


renderAddLocationEnglish :: MsgAddLocation -> Text
renderAddLocationEnglish MsgAddLocationName = "Name"

-- gen add form - end

-------------------------------------------------------
-- edit location
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
  (formWidget, _) <- generateFormPost $ vEditLocationForm $ Just location
  formLayout $ do
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
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            LocationName =. vEditLocationName vEditLocation
            , LocationVersion =. vEditLocationVersion vEditLocation + 1
            , LocationUpdatedAt =. curTime
            , LocationUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ updateWhereCount [ LocationId ==. locationId
                                              , LocationVersion ==. vEditLocationVersion vEditLocation
                                              ] persistFields
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
      { fsLabel = SomeMessage MsgEditLocationName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

data MsgEditLocation =
  MsgEditLocationName

instance RenderMessage App MsgEditLocation where
  renderMessage _ []        = renderEditLocationGerman
  renderMessage _ ("de":_) = renderEditLocationGerman
  renderMessage _ ("en":_) = renderEditLocationEnglish
  renderMessage _ ("en-US":_) = renderEditLocationEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderEditLocationGerman :: MsgEditLocation -> Text
renderEditLocationGerman MsgEditLocationName = "Name"


renderEditLocationEnglish :: MsgEditLocation -> Text
renderEditLocationEnglish MsgEditLocationName = "Name"

-- gen edit form - end

-------------------------------------------------------
-- delete location
-------------------------------------------------------

-- gen get delete form - start
getDeleteLocationFormR :: LocationId -> Handler Html
getDeleteLocationFormR locationId = do
  (formWidget, _) <- generateFormPost $ vDeleteLocationForm
  formLayout $ do
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

