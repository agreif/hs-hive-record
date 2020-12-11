{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Hive where

import Basement.IntegralConv
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Database.Persist.Sql (fromSqlKey, updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Hamlet (hamletFile)
import Prelude (read)

locationSelectField :: Field Handler (Key Location)
locationSelectField =
  selectField $ optionsPersistKey [] [Asc LocationName] locationName

-------------------------------------------------------
-- overview
-------------------------------------------------------

getHiveOverviewR :: Handler Html
getHiveOverviewR = do
  let route = HiverecR HiveOverviewPageDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getHiveOverviewPageDataR :: Handler Value
getHiveOverviewPageDataR = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  mainNavItems <- mainNavData user MainNavLocations
  urlRenderer <- getUrlRender
  hiveOverviewJDatas <- getHiveOverviewJDatas
  let pages =
        defaultDataPages
          { jDataPageHiveOverview =
              Just $
                JDataPageHiveOverview
                  { jDataPageHiveOverviewHives = hiveOverviewJDatas
                  }
          }
  msgHome <- localizedMsg MsgGlobalHome
  msgHiveOverview <- localizedMsg MsgGlobalHiveOverview
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ HiverecR HiveOverviewPageDataR
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
              { jDataHistoryStateUrl = urlRenderer $ HiverecR HiveOverviewR,
                jDataHistoryStateTitle = msgHiveOverview
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataR
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHiveOverview,
                jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl,
        jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
      }

getHiveOverviewJDatas :: Handler [JDataHiveOverviewHive]
getHiveOverviewJDatas = do
  urlRenderer <- getUrlRender
  hiveEnts <- runDB $ selectList [HiveIsDissolved ==. False] [Asc HiveName]
  forM hiveEnts $ \hiveEnt@(Entity hiveId hive) -> do
    inspectionTuples <- hiveDetailInspectionJDatas hiveId 2
    return $
      JDataHiveOverviewHive
        { jDataHiveOverviewHiveEnt = hiveEnt,
          jDataHiveOverviewHiveInspections =
            map
              ( \(inspectionId, inspectionJdata) ->
                  JDataHiveOverviewHiveInspection
                    { jDataHiveOverviewHiveInspection = inspectionJdata,
                      jDataHiveOverviewHiveInspectionEditFormUrl = urlRenderer $ HiverecR $ HiveOverviewEditInspectionFormR inspectionId
                    }
              )
              inspectionTuples,
          jDataHiveOverviewInspectionAddFormUrl = urlRenderer $ HiverecR $ HiveOverviewAddInspectionFormR hiveId,
          jDataHiveOverviewHiveDetailDataUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId,
          jDataHiveOverviewQueenColor = calcQueenColor $ hiveQueenYear hive
        }

-------------------------------------------------------
-- detail
-------------------------------------------------------

getHiveDetailR :: HiveId -> Handler Html
getHiveDetailR hiveId = do
  let route = HiverecR $ HiveDetailPageDataR hiveId
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getHiveDetailPageDataR :: HiveId -> Handler Value
getHiveDetailPageDataR hiveId = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  mainNavItems <- mainNavData user MainNavLocations
  hive <- runDB $ get404 hiveId
  let locationId = hiveLocationId hive
  location <- runDB $ get404 locationId
  urlRenderer <- getUrlRender
  visiblePagesCount <- inspectionVisiblePagesCount hiveId
  dbPagesCount <- runDB $ inspectionDbPagesCount hiveId
  jDataInspections' <-
    hiveDetailInspectionJDatas hiveId $
      visiblePagesCount * hiveDetailPageInspectionPageSize
  let jDataInspections = map snd jDataInspections'
  let pages =
        defaultDataPages
          { jDataPageHiveDetail =
              Just $
                JDataPageHiveDetail
                  { jDataPageHiveDetailHiveEnt = Entity hiveId hive,
                    jDataPageHiveDetailHiveEditFormUrl = urlRenderer $ HiverecR $ EditHiveFormR hiveId,
                    jDataPageHiveDetailInspections = jDataInspections,
                    jDataPageHiveDetailShowLessInspectionsUrl = if visiblePagesCount > 1 then Just $ urlRenderer $ HiverecR $ HiveDetailPageShowLessInspectionsR hiveId else Nothing,
                    jDataPageHiveDetailShowMoreInspectionsUrl = if visiblePagesCount < dbPagesCount then Just $ urlRenderer $ HiverecR $ HiveDetailPageShowMoreInspectionsR hiveId else Nothing,
                    jDataPageHiveDetailShowAllInspectionsUrl = if visiblePagesCount /= dbPagesCount then Just $ urlRenderer $ HiverecR $ HiveDetailPageShowAllInspectionsR hiveId else Nothing,
                    jDataPageHiveDetailInspectionAddFormUrl = urlRenderer $ HiverecR $ AddInspectionFormR hiveId,
                    jDataPageHiveDetailQueenColor = calcQueenColor $ hiveQueenYear hive
                  }
          }
  msgHome <- localizedMsg MsgGlobalHome
  msgLocations <- localizedMsg MsgGlobalLocations
  msgHive <- localizedMsg MsgGlobalHive
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentPageDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId
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
              { jDataHistoryStateUrl = urlRenderer $ HiverecR $ HiveDetailR hiveId,
                jDataHistoryStateTitle = msgHive
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataR
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgLocations,
                jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR LocationListPageDataR
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = locationName location,
                jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR $ LocationDetailPageDataR locationId
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = hiveName hive,
                jDataBreadcrumbItemDataUrl = currentPageDataJsonUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentPageDataJsonUrl,
        jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentPageDataJsonUrl
      }

postHiveDetailPageShowLessInspectionsR :: HiveId -> Handler Value
postHiveDetailPageShowLessInspectionsR hiveId = do
  urlRenderer <- getUrlRender
  pagesCount <- inspectionVisiblePagesCount hiveId
  when (pagesCount > 1)
    $ setSession (sessionKeyHiveDetailPageInspectionPages hiveId)
    $ (pack . show . pred) pagesCount
  returnJson $
    VPostSubmitSuccess
      { fsPostSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId
      }

postHiveDetailPageShowMoreInspectionsR :: HiveId -> Handler Value
postHiveDetailPageShowMoreInspectionsR hiveId = do
  urlRenderer <- getUrlRender
  pagesCount <- inspectionVisiblePagesCount hiveId
  setSession (sessionKeyHiveDetailPageInspectionPages hiveId) $ (pack . show . succ) pagesCount
  returnJson $
    VPostSubmitSuccess
      { fsPostSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId
      }

postHiveDetailPageShowAllInspectionsR :: HiveId -> Handler Value
postHiveDetailPageShowAllInspectionsR hiveId = do
  urlRenderer <- getUrlRender
  pagesCount <- runDB $ inspectionDbPagesCount hiveId
  setSession (sessionKeyHiveDetailPageInspectionPages hiveId) $
    (pack . show) pagesCount
  returnJson $
    VPostSubmitSuccess
      { fsPostSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId
      }

inspectionVisiblePagesCount :: HiveId -> Handler Int
inspectionVisiblePagesCount hiveId = do
  maybePagesCount <- lookupSession $ sessionKeyHiveDetailPageInspectionPages hiveId
  return $
    case maybePagesCount of
      Nothing -> 1
      Just pageCountStr -> read $ unpack pageCountStr

inspectionDbPagesCount :: HiveId -> YesodDB App Int
inspectionDbPagesCount hiveId = do
  c <- count [InspectionHiveId ==. hiveId]
  return $ paginationPagesCount c hiveDetailPageInspectionPageSize

sessionKeyHiveDetailPageInspectionPages :: HiveId -> Text
sessionKeyHiveDetailPageInspectionPages hiveId = concat ["hiveDetailPageInspectionPages-", pack $ show $ fromSqlKey hiveId]

hiveDetailPageInspectionPageSize :: Int
hiveDetailPageInspectionPageSize = 10

hiveDetailInspectionJDatas :: HiveId -> Int -> Handler [(InspectionId, JDataInspection)]
hiveDetailInspectionJDatas hiveId maybeLimitCount = do
  urlRenderer <- getUrlRender
  inspectionEntTuples <- runDB loadInspectionListTuples
  return $
    map
      ( \(inspectionEnt@(Entity inspectionId _), swarmingTypeEnt, inspectionfileEnts) ->
          ( inspectionId,
            JDataInspection
              { jDataInspectionEnt = inspectionEnt,
                jDataInspectionSwarmingTypeEnt = swarmingTypeEnt,
                jDataInspectionEditFormUrl = urlRenderer $ HiverecR $ EditInspectionFormR inspectionId,
                jDataInspectionDeleteFormUrl = urlRenderer $ HiverecR $ DeleteInspectionFormR inspectionId,
                jDataInspectionInspectionfileAddFormUrl = urlRenderer $ HiverecR $ AddInspectionfileFormR inspectionId,
                jDataInspectionInspectionfiles = getInspectionfileJDatas inspectionfileEnts urlRenderer
              }
          )
      )
      inspectionEntTuples
  where
    loadInspectionListTuples :: YesodDB App [(Entity Inspection, Entity SwarmingType, [Entity Inspectionfile])]
    loadInspectionListTuples = do
      tuples <- E.select $ E.from $ \(h `E.InnerJoin` i `E.InnerJoin` st) -> do
        E.on (i E.^. InspectionSwarmingTypeId E.==. st E.^. SwarmingTypeId)
        E.on (h E.^. HiveId E.==. i E.^. InspectionHiveId)
        E.where_ (h E.^. HiveId E.==. E.val hiveId)
        E.limit $ intToInt64 maybeLimitCount
        E.orderBy [E.desc (i E.^. InspectionDate)]
        return (i, st)
      forM
        (L.reverse tuples)
        ( \(inspectionEnt@(Entity inspectionId _), st) -> do
            inspectionfileEnts <- selectList [InspectionfileInspectionId ==. inspectionId] []
            return (inspectionEnt, st, inspectionfileEnts)
        )

getInspectionfileJDatas :: [Entity Inspectionfile] -> (Route App -> Text) -> [JDataInspectionfile]
getInspectionfileJDatas inspectionfileEnts urlRenderer =
  map
    ( \inspectionfileEnt@(Entity inspectionfileId _) ->
        JDataInspectionfile
          { jDataInspectionfileEnt = inspectionfileEnt,
            jDataInspectionfileEditFormUrl = urlRenderer $ HiverecR $ EditInspectionfileFormR inspectionfileId,
            jDataInspectionfileDeleteFormUrl = urlRenderer $ HiverecR $ DeleteInspectionfileFormR inspectionfileId,
            jDataInspectionfileDownloadUrl = urlRenderer $ HiverecR $ DownloadInspectionfileR inspectionfileId
          }
    )
    inspectionfileEnts

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddHive = VAddHive
  { vAddHiveName :: Text,
    vAddHiveQueenYear :: Maybe Int,
    vAddHiveDescription :: Maybe Textarea,
    vAddHiveIsDissolved :: Bool
  }

-- gen data add - end

-- gen get add form - start
getAddHiveFormR :: LocationId -> Handler Html
getAddHiveFormR locationId = do
  (formWidget, _) <- generateFormPost $ vAddHiveForm Nothing Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalAddHive}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddHiveR locationId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

-- gen post add form - start
postAddHiveR :: LocationId -> Handler Value
postAddHiveR locationId = do
  ((result, formWidget), _) <- runFormPost $ vAddHiveForm Nothing Nothing
  case result of
    FormSuccess vAddHive -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let hive =
            Hive
              { hiveLocationId = locationId,
                hiveName = vAddHiveName vAddHive,
                hiveQueenYear = vAddHiveQueenYear vAddHive,
                hiveDescription = vAddHiveDescription vAddHive,
                hiveIsDissolved = vAddHiveIsDissolved vAddHive,
                hiveVersion = 1,
                hiveCreatedAt = curTime,
                hiveCreatedBy = userIdent authUser,
                hiveUpdatedAt = curTime,
                hiveUpdatedBy = userIdent authUser
              }
      runDB $ do
        _ <- insert hive
        return ()
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataR locationId}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post add form - end

-- gen add form - start
vAddHiveForm :: Maybe HiveId -> Maybe Hive -> Html -> MForm Handler (FormResult VAddHive, Widget)
vAddHiveForm maybeHiveId maybeHive extra = do
  (nameResult, nameView) <-
    mreq
      textField
      nameFs
      (hiveName <$> maybeHive)
  (queenYearResult, queenYearView) <-
    mopt
      intField
      queenYearFs
      (hiveQueenYear <$> maybeHive)
  (descriptionResult, descriptionView) <-
    mopt
      textareaField
      descriptionFs
      (hiveDescription <$> maybeHive)
  (isDissolvedResult, isDissolvedView) <-
    mreq
      checkBoxField
      isDissolvedFs
      (hiveIsDissolved <$> maybeHive)
  let vAddHiveResult = VAddHive <$> nameResult <*> queenYearResult <*> descriptionResult <*> isDissolvedResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #nameInputWidget .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label #nameInputLabel .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        <span #nameInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveNameInputInfo}
        $maybe err <- fvErrors nameView
          <br>
          <span #nameInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #queenYearInputWidget .uk-margin-small :not $ null $ fvErrors queenYearView:.uk-form-danger>
      <label #queenYearInputLabel .uk-form-label :not $ null $ fvErrors queenYearView:.uk-text-danger for=#{fvId queenYearView}>#{fvLabel queenYearView}
      <div .uk-form-controls>
        ^{fvInput queenYearView}
        <span #queenYearInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveQueenYearInputInfo}
        $maybe err <- fvErrors queenYearView
          <br>
          <span #queenYearInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #descriptionInputWidget .uk-margin-small :not $ null $ fvErrors descriptionView:.uk-form-danger>
      <label #descriptionInputLabel .uk-form-label :not $ null $ fvErrors descriptionView:.uk-text-danger for=#{fvId descriptionView}>#{fvLabel descriptionView}
      <div .uk-form-controls>
        ^{fvInput descriptionView}
        <span #descriptionInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveDescriptionInputInfo}
        $maybe err <- fvErrors descriptionView
          <br>
          <span #descriptionInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #isDissolvedInputWidget .uk-margin-small :not $ null $ fvErrors isDissolvedView:.uk-form-danger>
      <label #isDissolvedInputLabel .uk-form-label :not $ null $ fvErrors isDissolvedView:.uk-text-danger for=#{fvId isDissolvedView}>#{fvLabel isDissolvedView}
      <div .uk-form-controls>
        ^{fvInput isDissolvedView}
        <span #isDissolvedInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveIsDissolvedInputInfo}
        $maybe err <- fvErrors isDissolvedView
          <br>
          <span #isDissolvedInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vAddHiveResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveName,
          fsTooltip = Nothing,
          fsId = Just "name",
          fsName = Just "name",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    queenYearFs :: FieldSettings App
    queenYearFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveQueenYear,
          fsTooltip = Nothing,
          fsId = Just "queenYear",
          fsName = Just "queenYear",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    descriptionFs :: FieldSettings App
    descriptionFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveDescription,
          fsTooltip = Nothing,
          fsId = Just "description",
          fsName = Just "description",
          fsAttrs = [("class", "uk-textarea uk-form-small uk-form-width-large"), ("rows", "5")]
        }
    isDissolvedFs :: FieldSettings App
    isDissolvedFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveIsDissolved,
          fsTooltip = Nothing,
          fsId = Just "isDissolved",
          fsName = Just "isDissolved",
          fsAttrs = [("class", "uk-checkbox")]
        }

-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditHive = VEditHive
  { vEditHiveLocationId :: LocationId,
    vEditHiveName :: Text,
    vEditHiveQueenYear :: Maybe Int,
    vEditHiveDescription :: Maybe Textarea,
    vEditHiveIsDissolved :: Bool,
    vEditHiveVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditHiveFormR :: HiveId -> Handler Html
getEditHiveFormR hiveId = do
  hive <- runDB $ get404 hiveId
  (formWidget, _) <- generateFormPost $ vEditHiveForm (Just hiveId) (Just hive)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalEditHive}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditHiveR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

-- gen post edit form - start
postEditHiveR :: HiveId -> Handler Value
postEditHiveR hiveId = do
  ((result, formWidget), _) <- runFormPost $ vEditHiveForm (Just hiveId) Nothing
  case result of
    FormSuccess vEditHive -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ HiveLocationId =. vEditHiveLocationId vEditHive,
              HiveName =. vEditHiveName vEditHive,
              HiveQueenYear =. vEditHiveQueenYear vEditHive,
              HiveDescription =. vEditHiveDescription vEditHive,
              HiveIsDissolved =. vEditHiveIsDissolved vEditHive,
              HiveVersion =. vEditHiveVersion vEditHive + 1,
              HiveUpdatedAt =. curTime,
              HiveUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ HiveId ==. hiveId,
              HiveVersion ==. vEditHiveVersion vEditHive
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR hiveId}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit form - end

-- gen edit form - start
vEditHiveForm :: Maybe HiveId -> Maybe Hive -> Html -> MForm Handler (FormResult VEditHive, Widget)
vEditHiveForm maybeHiveId maybeHive extra = do
  (locationIdResult, locationIdView) <-
    mreq
      locationSelectField
      locationIdFs
      (hiveLocationId <$> maybeHive)
  (nameResult, nameView) <-
    mreq
      textField
      nameFs
      (hiveName <$> maybeHive)
  (queenYearResult, queenYearView) <-
    mopt
      intField
      queenYearFs
      (hiveQueenYear <$> maybeHive)
  (descriptionResult, descriptionView) <-
    mopt
      textareaField
      descriptionFs
      (hiveDescription <$> maybeHive)
  (isDissolvedResult, isDissolvedView) <-
    mreq
      checkBoxField
      isDissolvedFs
      (hiveIsDissolved <$> maybeHive)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (hiveVersion <$> maybeHive)
  let vEditHiveResult = VEditHive <$> locationIdResult <*> nameResult <*> queenYearResult <*> descriptionResult <*> isDissolvedResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div #locationIdInputWidget .uk-margin-small :not $ null $ fvErrors locationIdView:.uk-form-danger>
      <label #locationIdInputLabel .uk-form-label :not $ null $ fvErrors locationIdView:.uk-text-danger for=#{fvId locationIdView}>#{fvLabel locationIdView}
      <div .uk-form-controls>
        ^{fvInput locationIdView}
        <span #locationIdInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveLocationIdInputInfo}
        $maybe err <- fvErrors locationIdView
          <br>
          <span #locationIdInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #nameInputWidget .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label #nameInputLabel .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        <span #nameInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveNameInputInfo}
        $maybe err <- fvErrors nameView
          <br>
          <span #nameInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #queenYearInputWidget .uk-margin-small :not $ null $ fvErrors queenYearView:.uk-form-danger>
      <label #queenYearInputLabel .uk-form-label :not $ null $ fvErrors queenYearView:.uk-text-danger for=#{fvId queenYearView}>#{fvLabel queenYearView}
      <div .uk-form-controls>
        ^{fvInput queenYearView}
        <span #queenYearInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveQueenYearInputInfo}
        $maybe err <- fvErrors queenYearView
          <br>
          <span #queenYearInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #descriptionInputWidget .uk-margin-small :not $ null $ fvErrors descriptionView:.uk-form-danger>
      <label #descriptionInputLabel .uk-form-label :not $ null $ fvErrors descriptionView:.uk-text-danger for=#{fvId descriptionView}>#{fvLabel descriptionView}
      <div .uk-form-controls>
        ^{fvInput descriptionView}
        <span #descriptionInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveDescriptionInputInfo}
        $maybe err <- fvErrors descriptionView
          <br>
          <span #descriptionInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #isDissolvedInputWidget .uk-margin-small :not $ null $ fvErrors isDissolvedView:.uk-form-danger>
      <label #isDissolvedInputLabel .uk-form-label :not $ null $ fvErrors isDissolvedView:.uk-text-danger for=#{fvId isDissolvedView}>#{fvLabel isDissolvedView}
      <div .uk-form-controls>
        ^{fvInput isDissolvedView}
        <span #isDissolvedInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgHiveIsDissolvedInputInfo}
        $maybe err <- fvErrors isDissolvedView
          <br>
          <span #isDissolvedInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vEditHiveResult, formWidget)
  where
    locationIdFs :: FieldSettings App
    locationIdFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveLocationId,
          fsTooltip = Nothing,
          fsId = Just "locationId",
          fsName = Just "locationId",
          fsAttrs = []
        }
    nameFs :: FieldSettings App
    nameFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveName,
          fsTooltip = Nothing,
          fsId = Just "name",
          fsName = Just "name",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    queenYearFs :: FieldSettings App
    queenYearFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveQueenYear,
          fsTooltip = Nothing,
          fsId = Just "queenYear",
          fsName = Just "queenYear",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    descriptionFs :: FieldSettings App
    descriptionFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveDescription,
          fsTooltip = Nothing,
          fsId = Just "description",
          fsName = Just "description",
          fsAttrs = [("class", "uk-textarea uk-form-small uk-form-width-large"), ("rows", "5")]
        }
    isDissolvedFs :: FieldSettings App
    isDissolvedFs =
      FieldSettings
        { fsLabel = SomeMessage MsgHiveIsDissolved,
          fsTooltip = Nothing,
          fsId = Just "isDissolved",
          fsName = Just "isDissolved",
          fsAttrs = [("class", "uk-checkbox")]
        }
    versionFs :: FieldSettings App
    versionFs =
      FieldSettings
        { fsLabel = "",
          fsTooltip = Nothing,
          fsId = Just "version",
          fsName = Just "version",
          fsAttrs = []
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
    toWidget
      [whamlet|
      <h1>_{MsgGlobalDeleteHive}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteHiveR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

-- gen post delete form - start
postDeleteHiveR :: HiveId -> Handler Value
postDeleteHiveR hiveId = do
  curTime <- liftIO getCurrentTime
  Entity _ authUser <- requireAuth
  hive <- runDB $ get404 hiveId
  runDB $ do
    -- trick to record the user deleting the entity
    updateWhere
      [HiveId ==. hiveId]
      [ HiveUpdatedAt =. curTime,
        HiveUpdatedBy =. userIdent authUser
      ]
    delete hiveId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ LocationDetailPageDataR $ hiveLocationId hive}

-- gen post delete form - end

-- gen delete form - start
vDeleteHiveForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteHiveForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
