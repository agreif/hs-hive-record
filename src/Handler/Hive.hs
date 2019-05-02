{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Hive where

import Handler.Common
import Import
import qualified Database.Esqueleto as E
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

locationSelectField :: Field Handler (Key Location)
locationSelectField =
  selectField $ optionsPersistKey [] [Asc LocationName] locationName

-------------------------------------------------------
-- detail
-------------------------------------------------------

getHiveDetailR :: HiveId -> Handler Html
getHiveDetailR hiveId =
  defaultLayout $
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
  appName <- runDB configAppName
  mainNavItems <- mainNavData user MainNavLocations
  hive <- runDB $ get404 hiveId
  let locationId = hiveLocationId hive
  location <- runDB $ get404 locationId
  urlRenderer <- getUrlRender
  jDataInspections <- hiveDetailInspectionJDatas hiveId
  let pages =
        defaultDataPages
        { jDataPageHiveDetail =
            Just $ JDataPageHiveDetail
            { jDataPageHiveDetailHiveEnt = Entity hiveId hive
            , jDataPageHiveDetailHiveEditFormUrl = urlRenderer $ HiverecR $ EditHiveFormR hiveId
            , jDataPageHiveDetailInspections = jDataInspections
            , jDataPageHiveDetailInspectionAddFormUrl = urlRenderer $ HiverecR $ AddInspectionFormR hiveId
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

hiveDetailInspectionJDatas :: HiveId -> Handler [JDataInspection]
hiveDetailInspectionJDatas hiveId = do
  urlRenderer <- getUrlRender
  inspectionEntTuples <- runDB $ loadInspectionListTuples hiveId
  return $ map
    (\(inspectionEnt@(Entity inspectionId _), temperTypeEnt, runningTypeEnt, swarmingTypeEnt, inspectionfileEnts) ->
       JDataInspection
       { jDataInspectionEnt = inspectionEnt
       , jDataInspectionTemperTypeEnt = temperTypeEnt
       , jDataInspectionRunningTypeEnt = runningTypeEnt
       , jDataInspectionSwarmingTypeEnt = swarmingTypeEnt
       , jDataInspectionEditFormUrl = urlRenderer $ HiverecR $ EditInspectionFormR inspectionId
       , jDataInspectionDeleteFormUrl = urlRenderer $ HiverecR $ DeleteInspectionFormR inspectionId
       , jDataInspectionInspectionfileAddFormUrl = urlRenderer $ HiverecR $ AddInspectionfileFormR inspectionId
       , jDataInspectionInspectionfiles = getInspectionfileJDatas inspectionfileEnts urlRenderer
       })
    inspectionEntTuples

getInspectionfileJDatas :: [Entity Inspectionfile] -> (Route App -> Text) -> [JDataInspectionfile]
getInspectionfileJDatas inspectionfileEnts urlRenderer =
  map
  (\inspectionfileEnt@(Entity inspectionfileId _) ->
      JDataInspectionfile
      { jDataInspectionfileEnt = inspectionfileEnt
      , jDataInspectionfileEditFormUrl = urlRenderer $ HiverecR $ EditInspectionfileFormR inspectionfileId
      , jDataInspectionfileDeleteFormUrl = urlRenderer $ HiverecR $ DeleteInspectionfileFormR inspectionfileId
      , jDataInspectionfileDownloadUrl = urlRenderer $ HiverecR $ DownloadInspectionfileR inspectionfileId
      })
  inspectionfileEnts


loadInspectionListTuples :: HiveId -> YesodDB App [(Entity Inspection, Entity TemperType, Entity RunningType, Entity SwarmingType, [Entity Inspectionfile])]
loadInspectionListTuples hiveId = do
  tuples <- E.select $ E.from $ \(h `E.InnerJoin` i `E.InnerJoin` tt `E.InnerJoin` rt `E.InnerJoin` st) -> do
    E.on (i E.^. InspectionSwarmingTypeId E.==. st E.^. SwarmingTypeId)
    E.on (i E.^. InspectionRunningTypeId E.==. rt E.^. RunningTypeId)
    E.on (i E.^. InspectionTemperTypeId E.==. tt E.^. TemperTypeId)
    E.on (h E.^. HiveId E.==. i E.^. InspectionHiveId)
    E.where_ (h E.^. HiveId E.==. E.val hiveId)
    E.orderBy [E.asc (i E.^. InspectionDate)]
    return (i, tt, rt, st)
  tuples' <- forM tuples
             (\(inspectionEnt@(Entity inspectionId _), tt, rt, st) -> do
                 inspectionfileEnts <- selectList [InspectionfileInspectionId ==. inspectionId] []
                 return (inspectionEnt, tt, rt, st, inspectionfileEnts)
             )
  return tuples'

-------------------------------------------------------
-- add
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
  formLayout $
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
      runDB $ do
        _ <- insert hive
        return ()
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
      { fsLabel = SomeMessage MsgHiveName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    descriptionFs :: FieldSettings App
    descriptionFs = FieldSettings
      { fsLabel = SomeMessage MsgHiveDescription
      , fsTooltip = Nothing
      , fsId = Just "description"
      , fsName = Just "description"
      , fsAttrs = [ ("class","uk-textarea uk-form-small uk-form-width-large"), ("rows","5") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditHive = VEditHive
  { vEditHiveLocationId :: LocationId
  , vEditHiveName :: Text
  , vEditHiveDescription :: Maybe Textarea
  , vEditHiveVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditHiveFormR :: HiveId -> Handler Html
getEditHiveFormR hiveId = do
  hive <- runDB $ get404 hiveId
  (formWidget, _) <- generateFormPost $ vEditHiveForm (Just hive)
  formLayout $
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
            HiveLocationId =. vEditHiveLocationId vEditHive
            , HiveName =. vEditHiveName vEditHive
            , HiveDescription =. vEditHiveDescription vEditHive
            , HiveVersion =. vEditHiveVersion vEditHive + 1
            , HiveUpdatedAt =. curTime
            , HiveUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ HiveId ==. hiveId
                               , HiveVersion ==. vEditHiveVersion vEditHive
                               ] persistFields
        return uc
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
  (locationIdResult, locationIdView) <- mreq locationSelectField
    locationIdFs
    (hiveLocationId <$> maybeHive)
  (nameResult, nameView) <- mreq textField
    nameFs
    (hiveName <$> maybeHive)
  (descriptionResult, descriptionView) <- mopt textareaField
    descriptionFs
    (hiveDescription <$> maybeHive)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (hiveVersion <$> maybeHive)
  let vEditHiveResult = VEditHive <$> locationIdResult <*> nameResult <*> descriptionResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors locationIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors locationIdView:.uk-text-danger for=#{fvId locationIdView}>#{fvLabel locationIdView}
      <div .uk-form-controls>
        ^{fvInput locationIdView}
        $maybe err <- fvErrors locationIdView
          &nbsp;#{err}
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
    locationIdFs :: FieldSettings App
    locationIdFs = FieldSettings
      { fsLabel = SomeMessage MsgHiveLocationId
      , fsTooltip = Nothing
      , fsId = Just "locationId"
      , fsName = Just "locationId"
      , fsAttrs = [  ]
      }
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgHiveName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    descriptionFs :: FieldSettings App
    descriptionFs = FieldSettings
      { fsLabel = SomeMessage MsgHiveDescription
      , fsTooltip = Nothing
      , fsId = Just "description"
      , fsName = Just "description"
      , fsAttrs = [ ("class","uk-textarea uk-form-small uk-form-width-large"), ("rows","5") ]
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
getDeleteHiveFormR :: HiveId -> Handler Html
getDeleteHiveFormR hiveId = do
  (formWidget, _) <- generateFormPost $ vDeleteHiveForm
  formLayout $
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
