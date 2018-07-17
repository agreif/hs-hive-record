{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Hive where

import Handler.Common
import Import
--import qualified Database.Esqueleto as E
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI






-------------------------------------------------------
-- hive detail page
-------------------------------------------------------

getHiveDetailR :: HiveId -> Handler Html
getHiveDetailR hiveId = defaultLayout $ do
  toWidget [whamlet|
                   <body-tag>
                   <script>
                     \ riot.compile(function() {
                     \   bodyTag = riot.mount('body-tag')[0]
                     \   bodyTag.refreshData("@{HiverecR $ HiveDetailPageDataJsonR hiveId}")
                     \ })
                   |]

getHiveDetailPageDataJsonR :: HiveId -> Handler Value
getHiveDetailPageDataJsonR hiveId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB $ configAppName
  mainNavItems <- mainNavData user MainNavLocation
  hive <- runDB $ get404 hiveId
  let locationId = hiveLocationId hive
  location <- runDB $ get404 locationId
  urlRenderer <- getUrlRender
  let pages =
        defaultDataPages
        { jDataPageHiveDetail =
            Just $ JDataPageHiveDetail
            { jDataPageHiveDetailHiveEnt = Entity hiveId hive
            , jDataPageHiveDetailHiveEditFormUrl = urlRenderer $ HiverecR $ EditHiveFormR hiveId
            }
        }
  msgHome <- localizedMsg MsgGlobalHome
  msgLocations <- localizedMsg MsgGlobalLocations
  msgHive <- localizedMsg MsgGlobalHive
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId
  returnJson JData
    { jDataAppName = appName
    , jDataUserIdent = userIdent user
    , jDataMainNavItems = mainNavItems
    , jDataSubNavItems = []
    , jDataPages = pages
    , jDataHistoryState = Just JDataHistoryState
      { jDataHistoryStateUrl = urlRenderer $ HiverecR $ HiveDetailR hiveId
      , jDataHistoryStateTitle = msgHive
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
                               , jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId }
                             , JDataBreadcrumbItem
                               { jDataBreadcrumbItemLabel = hiveName hive
                               , jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl }
                             ]
    , jDataCurrentLanguage = currentLanguage
    , jDataTranslation = translation
    , jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl
    , jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
    }







-------------------------------------------------------
-- add hive
-------------------------------------------------------

-- gen data add - start
data VAddHive = VAddHive
  { vAddHiveName :: Text
  , vAddHiveDescription :: Maybe Textarea
  }
-- gen data add - end

-- gen get add form - start
getAddHiveFormR :: LocationId -> Handler Html
getAddHiveFormR locationId = do
  (formWidget, _) <- generateFormPost $ vAddHiveForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalAddHive}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddHiveR locationId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add form - start
postAddHiveR :: LocationId -> Handler Value
postAddHiveR locationId = do
  ((result, formWidget), _) <- runFormPost $ vAddHiveForm Nothing
  case result of
    FormSuccess vAddHive -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let hive = Hive
            {
            hiveLocationId = locationId
            , hiveName = vAddHiveName vAddHive
            , hiveDescription = vAddHiveDescription vAddHive
            , hiveVersion = 1
            , hiveCreatedAt = curTime
            , hiveCreatedBy = userIdent authUser
            , hiveUpdatedAt = curTime
            , hiveUpdatedBy = userIdent authUser
            }
      _ <- runDB $ insert hive
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR locationId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add form - end

-- gen add form - start
vAddHiveForm :: Maybe Hive -> Html -> MForm Handler (FormResult VAddHive, Widget)
vAddHiveForm maybeHive extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (hiveName <$> maybeHive)
  (descriptionResult, descriptionView) <- mopt textareaField
    descriptionFs
    (hiveDescription <$> maybeHive)
  let vAddHiveResult = VAddHive <$> nameResult <*> descriptionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors descriptionView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors descriptionView:.uk-text-danger for=#{fvId descriptionView}>#{fvLabel descriptionView}
      <div .uk-form-controls>
        ^{fvInput descriptionView}
        $maybe err <- fvErrors descriptionView
          &nbsp;#{err}
    |]
  return (vAddHiveResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgAddHiveName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    descriptionFs :: FieldSettings App
    descriptionFs = FieldSettings
      { fsLabel = SomeMessage MsgAddHiveDescription
      , fsTooltip = Nothing
      , fsId = Just "description"
      , fsName = Just "description"
      , fsAttrs = [ ("class","uk-form-width-large uk-textarea uk-form-small"), ("rows","5") ]
      }

data MsgAddHive =
  MsgAddHiveName
  | MsgAddHiveDescription

instance RenderMessage App MsgAddHive where
  renderMessage _ []        = renderAddHiveGerman
  renderMessage _ ("de":_) = renderAddHiveGerman
  renderMessage _ ("en":_) = renderAddHiveEnglish
  renderMessage _ ("en-US":_) = renderAddHiveEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderAddHiveGerman :: MsgAddHive -> Text
renderAddHiveGerman MsgAddHiveName = "Name"
renderAddHiveGerman MsgAddHiveDescription = "Beschreibung"


renderAddHiveEnglish :: MsgAddHive -> Text
renderAddHiveEnglish MsgAddHiveName = "Name"
renderAddHiveEnglish MsgAddHiveDescription = "Description"

-- gen add form - end

-------------------------------------------------------
-- edit hive
-------------------------------------------------------

-- gen data edit - start
data VEditHive = VEditHive
  { vEditHiveName :: Text
  , vEditHiveDescription :: Maybe Textarea
  , vEditHiveVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditHiveFormR :: HiveId -> Handler Html
getEditHiveFormR hiveId = do
  hive <- runDB $ get404 hiveId
  (formWidget, _) <- generateFormPost $ vEditHiveForm $ Just hive
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalEditHive}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditHiveR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditHiveR :: HiveId -> Handler Value
postEditHiveR hiveId = do
  ((result, formWidget), _) <- runFormPost $ vEditHiveForm Nothing
  case result of
    FormSuccess vEditHive -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            HiveName =. vEditHiveName vEditHive
            , HiveDescription =. vEditHiveDescription vEditHive
            , HiveVersion =. vEditHiveVersion vEditHive + 1
            , HiveUpdatedAt =. curTime
            , HiveUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ updateWhereCount [ HiveId ==. hiveId
                                              , HiveVersion ==. vEditHiveVersion vEditHive
                                              ] persistFields
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR hiveId }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditHiveForm :: Maybe Hive -> Html -> MForm Handler (FormResult VEditHive, Widget)
vEditHiveForm maybeHive extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (hiveName <$> maybeHive)
  (descriptionResult, descriptionView) <- mopt textareaField
    descriptionFs
    (hiveDescription <$> maybeHive)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (hiveVersion <$> maybeHive)
  let vEditHiveResult = VEditHive <$> nameResult <*> descriptionResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors descriptionView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors descriptionView:.uk-text-danger for=#{fvId descriptionView}>#{fvLabel descriptionView}
      <div .uk-form-controls>
        ^{fvInput descriptionView}
        $maybe err <- fvErrors descriptionView
          &nbsp;#{err}
    |]
  return (vEditHiveResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgEditHiveName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    descriptionFs :: FieldSettings App
    descriptionFs = FieldSettings
      { fsLabel = SomeMessage MsgEditHiveDescription
      , fsTooltip = Nothing
      , fsId = Just "description"
      , fsName = Just "description"
      , fsAttrs = [ ("class","uk-form-width-large uk-textarea uk-form-small"), ("rows","5") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

data MsgEditHive =
  MsgEditHiveName
  | MsgEditHiveDescription

instance RenderMessage App MsgEditHive where
  renderMessage _ []        = renderEditHiveGerman
  renderMessage _ ("de":_) = renderEditHiveGerman
  renderMessage _ ("en":_) = renderEditHiveEnglish
  renderMessage _ ("en-US":_) = renderEditHiveEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderEditHiveGerman :: MsgEditHive -> Text
renderEditHiveGerman MsgEditHiveName = "Name"
renderEditHiveGerman MsgEditHiveDescription = "Beschreibung"


renderEditHiveEnglish :: MsgEditHive -> Text
renderEditHiveEnglish MsgEditHiveName = "Name"
renderEditHiveEnglish MsgEditHiveDescription = "Description"

-- gen edit form - end

-------------------------------------------------------
-- delete hive
-------------------------------------------------------

-- gen get delete form - start
getDeleteHiveFormR :: HiveId -> Handler Html
getDeleteHiveFormR hiveId = do
  (formWidget, _) <- generateFormPost $ vDeleteHiveForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalDeleteHive}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteHiveR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteHiveR :: HiveId -> Handler Value
postDeleteHiveR hiveId = do
  hive <- runDB $ get404 hiveId
  runDB $ delete hiveId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataJsonR $ hiveLocationId hive }
-- gen post delete form - end

-- gen delete form - start
vDeleteHiveForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteHiveForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
