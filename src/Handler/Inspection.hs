{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Inspection where

import qualified Data.List as L
import qualified Data.Maybe as M
import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

swarmingTypeSelectField :: Field Handler (Key SwarmingType)
swarmingTypeSelectField =
  selectField $ optionsPersistKey [] [Asc SwarmingTypeSortIndex] swarmingTypeName

-------------------------------------------------------
-- add
-------------------------------------------------------

defaultAddInspection :: HiveId -> Handler (Maybe Inspection)
defaultAddInspection hiveId = do
  now <- liftIO getCurrentTime
  maybeToday <- getInspectionDateFromSession
  today <- case maybeToday of
    Just today' -> return today'
    _ -> liftIO getCurrentDay
  maybeSwarmingTypeId <- runDB defaultSwarmingTypeId
  maybeLastInspectionEnt <- runDB $ getLastInspectionEnt hiveId
  case maybeLastInspectionEnt of
    Just (Entity _ inspection) ->
      return $ Just $
        Inspection
          { inspectionHiveId = hiveId,
            inspectionDate = today,
            inspectionSwarmingTypeId = inspectionSwarmingTypeId inspection,
            inspectionQueenSeen = False,
            inspectionBeeCoveredFrames = inspectionBeeCoveredFrames inspection,
            inspectionBroodFrames = inspectionBroodFrames inspection,
            inspectionHoneyFrames = inspectionHoneyFrames inspection,
            inspectionTreatment = Nothing,
            inspectionFeeding = Nothing,
            inspectionNotes = Nothing,
            inspectionVersion = 1,
            inspectionCreatedAt = now,
            inspectionCreatedBy = dbSystemUser,
            inspectionUpdatedAt = now,
            inspectionUpdatedBy = dbSystemUser
          }
    _ ->
      return $
        if M.isNothing maybeSwarmingTypeId
          then Nothing
          else
            Just $
              Inspection
                { inspectionHiveId = hiveId,
                  inspectionDate = today,
                  inspectionSwarmingTypeId = M.fromJust maybeSwarmingTypeId,
                  inspectionQueenSeen = False,
                  inspectionBeeCoveredFrames = 0,
                  inspectionBroodFrames = 0,
                  inspectionHoneyFrames = 0,
                  inspectionTreatment = Nothing,
                  inspectionFeeding = Nothing,
                  inspectionNotes = Nothing,
                  inspectionVersion = 1,
                  inspectionCreatedAt = now,
                  inspectionCreatedBy = dbSystemUser,
                  inspectionUpdatedAt = now,
                  inspectionUpdatedBy = dbSystemUser
                }
  where
    defaultSwarmingTypeId :: YesodDB App (Maybe SwarmingTypeId)
    defaultSwarmingTypeId = do
      swarmingTypeEnts <- selectList ([] :: [Filter SwarmingType]) []
      if L.null swarmingTypeEnts
        then return Nothing
        else
          return
            $ Just
            $ snd
            $ L.head
            $ L.sort
            $ L.map (\(Entity stId st) -> (swarmingTypeSortIndex st, stId)) swarmingTypeEnts

-- gen data add - start
data VAddInspection = VAddInspection
  { vAddInspectionDate :: Day,
    vAddInspectionSwarmingTypeId :: SwarmingTypeId,
    vAddInspectionQueenSeen :: Bool,
    vAddInspectionBeeCoveredFrames :: Int,
    vAddInspectionBroodFrames :: Int,
    vAddInspectionHoneyFrames :: Int,
    vAddInspectionTreatment :: Maybe Text,
    vAddInspectionFeeding :: Maybe Text,
    vAddInspectionNotes :: Maybe Textarea
  }

-- gen data add - end

-- gen get add form - start
getAddInspectionFormR :: HiveId -> Handler Html
getAddInspectionFormR hiveId = do
  defaultMaybeAddModel <- defaultAddInspection hiveId
  (formWidget, _) <- generateFormPost $ vAddInspectionForm defaultMaybeAddModel
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalAddInspection}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddInspectionR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

getHiveOverviewAddInspectionFormR :: HiveId -> Handler Html
getHiveOverviewAddInspectionFormR hiveId = do
  defaultMaybeAddModel <- defaultAddInspection hiveId
  (formWidget, _) <- generateFormPost $ vAddInspectionForm defaultMaybeAddModel
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalAddInspection}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ HiveOverviewAddInspectionR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

inspectionDateSessionKey :: Text
inspectionDateSessionKey = "inspectionDate"

storeInspectionDateToSession :: VAddInspection -> InspectionId -> YesodDB App ()
storeInspectionDateToSession vAddInspection _ = do
  let dateStr = formatDay $ vAddInspectionDate vAddInspection
  setSession inspectionDateSessionKey dateStr
  return ()

getInspectionDateFromSession :: Handler (Maybe Day)
getInspectionDateFromSession = do
  maybeDateText <- lookupSession inspectionDateSessionKey
  case maybeDateText of
    Just dateText -> do
      day <- parseDay dateText
      return $ Just day
    _ -> return Nothing

-- gen post add form - start
postAddInspectionR :: HiveId -> Handler Value
postAddInspectionR hiveId = do
  ((result, formWidget), _) <- runFormPost $ vAddInspectionForm Nothing
  case result of
    FormSuccess vAddInspection -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let inspection =
            Inspection
              { inspectionHiveId = hiveId,
                inspectionDate = vAddInspectionDate vAddInspection,
                inspectionSwarmingTypeId = vAddInspectionSwarmingTypeId vAddInspection,
                inspectionQueenSeen = vAddInspectionQueenSeen vAddInspection,
                inspectionBeeCoveredFrames = vAddInspectionBeeCoveredFrames vAddInspection,
                inspectionBroodFrames = vAddInspectionBroodFrames vAddInspection,
                inspectionHoneyFrames = vAddInspectionHoneyFrames vAddInspection,
                inspectionTreatment = vAddInspectionTreatment vAddInspection,
                inspectionFeeding = vAddInspectionFeeding vAddInspection,
                inspectionNotes = vAddInspectionNotes vAddInspection,
                inspectionVersion = 1,
                inspectionCreatedAt = curTime,
                inspectionCreatedBy = userIdent authUser,
                inspectionUpdatedAt = curTime,
                inspectionUpdatedBy = userIdent authUser
              }
      runDB $ do
        inspectionId <- insert inspection
        storeInspectionDateToSession vAddInspection inspectionId
        return ()
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ getAddInspectionSuccessDataJsonUrl inspection maybeCurRoute}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post add form - end

postHiveOverviewAddInspectionR :: HiveId -> Handler Value
postHiveOverviewAddInspectionR = postAddInspectionR

getAddInspectionSuccessDataJsonUrl :: Inspection -> Maybe (Route App) -> Route App
getAddInspectionSuccessDataJsonUrl inspection maybeCurRoute =
  case maybeCurRoute of
    Just (HiverecR (HiveOverviewAddInspectionR _)) -> HiverecR HiveOverviewPageDataR
    _ -> HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection

-- gen add form - start
vAddInspectionForm :: Maybe Inspection -> Html -> MForm Handler (FormResult VAddInspection, Widget)
vAddInspectionForm maybeInspection extra = do
  (dateResult, dateView) <-
    mreq
      dayField
      dateFs
      (inspectionDate <$> maybeInspection)
  (swarmingTypeIdResult, swarmingTypeIdView) <-
    mreq
      swarmingTypeSelectField
      swarmingTypeIdFs
      (inspectionSwarmingTypeId <$> maybeInspection)
  (queenSeenResult, queenSeenView) <-
    mreq
      checkBoxField
      queenSeenFs
      (inspectionQueenSeen <$> maybeInspection)
  (beeCoveredFramesResult, beeCoveredFramesView) <-
    mreq
      intField
      beeCoveredFramesFs
      (inspectionBeeCoveredFrames <$> maybeInspection)
  (broodFramesResult, broodFramesView) <-
    mreq
      intField
      broodFramesFs
      (inspectionBroodFrames <$> maybeInspection)
  (honeyFramesResult, honeyFramesView) <-
    mreq
      intField
      honeyFramesFs
      (inspectionHoneyFrames <$> maybeInspection)
  (treatmentResult, treatmentView) <-
    mopt
      textField
      treatmentFs
      (inspectionTreatment <$> maybeInspection)
  (feedingResult, feedingView) <-
    mopt
      textField
      feedingFs
      (inspectionFeeding <$> maybeInspection)
  (notesResult, notesView) <-
    mopt
      textareaField
      notesFs
      (inspectionNotes <$> maybeInspection)
  let vAddInspectionResult = VAddInspection <$> dateResult <*> swarmingTypeIdResult <*> queenSeenResult <*> beeCoveredFramesResult <*> broodFramesResult <*> honeyFramesResult <*> treatmentResult <*> feedingResult <*> notesResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #dateInputWidget .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label #dateInputLabel .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        <span #dateInputError>
          $maybe err <- fvErrors dateView
            &nbsp;#{err}
    <div #swarmingTypeIdInputWidget .uk-margin-small :not $ null $ fvErrors swarmingTypeIdView:.uk-form-danger>
      <label #swarmingTypeIdInputLabel .uk-form-label :not $ null $ fvErrors swarmingTypeIdView:.uk-text-danger for=#{fvId swarmingTypeIdView}>#{fvLabel swarmingTypeIdView}
      <div .uk-form-controls>
        ^{fvInput swarmingTypeIdView}
        <span #swarmingTypeIdInputError>
          $maybe err <- fvErrors swarmingTypeIdView
            &nbsp;#{err}
    <div #queenSeenInputWidget .uk-margin-small :not $ null $ fvErrors queenSeenView:.uk-form-danger>
      <label #queenSeenInputLabel .uk-form-label :not $ null $ fvErrors queenSeenView:.uk-text-danger for=#{fvId queenSeenView}>#{fvLabel queenSeenView}
      <div .uk-form-controls>
        ^{fvInput queenSeenView}
        <span #queenSeenInputError>
          $maybe err <- fvErrors queenSeenView
            &nbsp;#{err}
    <div #beeCoveredFramesInputWidget .uk-margin-small :not $ null $ fvErrors beeCoveredFramesView:.uk-form-danger>
      <label #beeCoveredFramesInputLabel .uk-form-label :not $ null $ fvErrors beeCoveredFramesView:.uk-text-danger for=#{fvId beeCoveredFramesView}>#{fvLabel beeCoveredFramesView}
      <div .uk-form-controls>
        ^{fvInput beeCoveredFramesView}
        <span #beeCoveredFramesInputError>
          $maybe err <- fvErrors beeCoveredFramesView
            &nbsp;#{err}
    <div #broodFramesInputWidget .uk-margin-small :not $ null $ fvErrors broodFramesView:.uk-form-danger>
      <label #broodFramesInputLabel .uk-form-label :not $ null $ fvErrors broodFramesView:.uk-text-danger for=#{fvId broodFramesView}>#{fvLabel broodFramesView}
      <div .uk-form-controls>
        ^{fvInput broodFramesView}
        <span #broodFramesInputError>
          $maybe err <- fvErrors broodFramesView
            &nbsp;#{err}
    <div #honeyFramesInputWidget .uk-margin-small :not $ null $ fvErrors honeyFramesView:.uk-form-danger>
      <label #honeyFramesInputLabel .uk-form-label :not $ null $ fvErrors honeyFramesView:.uk-text-danger for=#{fvId honeyFramesView}>#{fvLabel honeyFramesView}
      <div .uk-form-controls>
        ^{fvInput honeyFramesView}
        <span #honeyFramesInputError>
          $maybe err <- fvErrors honeyFramesView
            &nbsp;#{err}
    <div #treatmentInputWidget .uk-margin-small :not $ null $ fvErrors treatmentView:.uk-form-danger>
      <label #treatmentInputLabel .uk-form-label :not $ null $ fvErrors treatmentView:.uk-text-danger for=#{fvId treatmentView}>#{fvLabel treatmentView}
      <div .uk-form-controls>
        ^{fvInput treatmentView}
        <span #treatmentInputError>
          $maybe err <- fvErrors treatmentView
            &nbsp;#{err}
    <div #feedingInputWidget .uk-margin-small :not $ null $ fvErrors feedingView:.uk-form-danger>
      <label #feedingInputLabel .uk-form-label :not $ null $ fvErrors feedingView:.uk-text-danger for=#{fvId feedingView}>#{fvLabel feedingView}
      <div .uk-form-controls>
        ^{fvInput feedingView}
        <span #feedingInputError>
          $maybe err <- fvErrors feedingView
            &nbsp;#{err}
    <div #notesInputWidget .uk-margin-small :not $ null $ fvErrors notesView:.uk-form-danger>
      <label #notesInputLabel .uk-form-label :not $ null $ fvErrors notesView:.uk-text-danger for=#{fvId notesView}>#{fvLabel notesView}
      <div .uk-form-controls>
        ^{fvInput notesView}
        <span #notesInputError>
          $maybe err <- fvErrors notesView
            &nbsp;#{err}
    |]
  return (vAddInspectionResult, formWidget)
  where
    dateFs :: FieldSettings App
    dateFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionDate,
          fsTooltip = Nothing,
          fsId = Just "date",
          fsName = Just "date",
          fsAttrs = []
        }
    swarmingTypeIdFs :: FieldSettings App
    swarmingTypeIdFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionSwarmingTypeId,
          fsTooltip = Nothing,
          fsId = Just "swarmingTypeId",
          fsName = Just "swarmingTypeId",
          fsAttrs = []
        }
    queenSeenFs :: FieldSettings App
    queenSeenFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionQueenSeen,
          fsTooltip = Nothing,
          fsId = Just "queenSeen",
          fsName = Just "queenSeen",
          fsAttrs = [("class", "uk-checkbox")]
        }
    beeCoveredFramesFs :: FieldSettings App
    beeCoveredFramesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionBeeCoveredFrames,
          fsTooltip = Nothing,
          fsId = Just "beeCoveredFrames",
          fsName = Just "beeCoveredFrames",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    broodFramesFs :: FieldSettings App
    broodFramesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionBroodFrames,
          fsTooltip = Nothing,
          fsId = Just "broodFrames",
          fsName = Just "broodFrames",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    honeyFramesFs :: FieldSettings App
    honeyFramesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionHoneyFrames,
          fsTooltip = Nothing,
          fsId = Just "honeyFrames",
          fsName = Just "honeyFrames",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    treatmentFs :: FieldSettings App
    treatmentFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionTreatment,
          fsTooltip = Nothing,
          fsId = Just "treatment",
          fsName = Just "treatment",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    feedingFs :: FieldSettings App
    feedingFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionFeeding,
          fsTooltip = Nothing,
          fsId = Just "feeding",
          fsName = Just "feeding",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    notesFs :: FieldSettings App
    notesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionNotes,
          fsTooltip = Nothing,
          fsId = Just "notes",
          fsName = Just "notes",
          fsAttrs = [("class", "uk-textarea uk-form-small uk-width-5-6"), ("rows", "10")]
        }

-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditInspection = VEditInspection
  { vEditInspectionDate :: Day,
    vEditInspectionSwarmingTypeId :: SwarmingTypeId,
    vEditInspectionQueenSeen :: Bool,
    vEditInspectionBeeCoveredFrames :: Int,
    vEditInspectionBroodFrames :: Int,
    vEditInspectionHoneyFrames :: Int,
    vEditInspectionTreatment :: Maybe Text,
    vEditInspectionFeeding :: Maybe Text,
    vEditInspectionNotes :: Maybe Textarea,
    vEditInspectionVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditInspectionFormR :: InspectionId -> Handler Html
getEditInspectionFormR inspectionId = do
  inspection <- runDB $ get404 inspectionId
  (formWidget, _) <- generateFormPost $ vEditInspectionForm (Just inspection)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalEditInspection}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditInspectionR inspectionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

getHiveOverviewEditInspectionFormR :: InspectionId -> Handler Html
getHiveOverviewEditInspectionFormR inspectionId = do
  inspection <- runDB $ get404 inspectionId
  (formWidget, _) <- generateFormPost $ vEditInspectionForm (Just inspection)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalEditInspection}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ HiveOverviewEditInspectionR inspectionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen post edit form - start
postEditInspectionR :: InspectionId -> Handler Value
postEditInspectionR inspectionId = do
  ((result, formWidget), _) <- runFormPost $ vEditInspectionForm Nothing
  case result of
    FormSuccess vEditInspection -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      inspection <- runDB $ get404 inspectionId
      let persistFields =
            [ InspectionDate =. vEditInspectionDate vEditInspection,
              InspectionSwarmingTypeId =. vEditInspectionSwarmingTypeId vEditInspection,
              InspectionQueenSeen =. vEditInspectionQueenSeen vEditInspection,
              InspectionBeeCoveredFrames =. vEditInspectionBeeCoveredFrames vEditInspection,
              InspectionBroodFrames =. vEditInspectionBroodFrames vEditInspection,
              InspectionHoneyFrames =. vEditInspectionHoneyFrames vEditInspection,
              InspectionTreatment =. vEditInspectionTreatment vEditInspection,
              InspectionFeeding =. vEditInspectionFeeding vEditInspection,
              InspectionNotes =. vEditInspectionNotes vEditInspection,
              InspectionVersion =. vEditInspectionVersion vEditInspection + 1,
              InspectionUpdatedAt =. curTime,
              InspectionUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ InspectionId ==. inspectionId,
              InspectionVersion ==. vEditInspectionVersion vEditInspection
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ getEditInspectionSuccessDataJsonUrl (inspectionHiveId inspection) maybeCurRoute}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ getEditInspectionSuccessDataJsonUrl (inspectionHiveId inspection) maybeCurRoute}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit form - end

postHiveOverviewEditInspectionR :: InspectionId -> Handler Value
postHiveOverviewEditInspectionR = postEditInspectionR

getEditInspectionSuccessDataJsonUrl :: HiveId -> Maybe (Route App) -> Route App
getEditInspectionSuccessDataJsonUrl hiveId maybeCurRoute =
  case maybeCurRoute of
    Just (HiverecR (HiveOverviewEditInspectionR _)) -> HiverecR HiveOverviewPageDataR
    _ -> HiverecR $ HiveDetailPageDataR hiveId

-- gen edit form - start
vEditInspectionForm :: Maybe Inspection -> Html -> MForm Handler (FormResult VEditInspection, Widget)
vEditInspectionForm maybeInspection extra = do
  (dateResult, dateView) <-
    mreq
      dayField
      dateFs
      (inspectionDate <$> maybeInspection)
  (swarmingTypeIdResult, swarmingTypeIdView) <-
    mreq
      swarmingTypeSelectField
      swarmingTypeIdFs
      (inspectionSwarmingTypeId <$> maybeInspection)
  (queenSeenResult, queenSeenView) <-
    mreq
      checkBoxField
      queenSeenFs
      (inspectionQueenSeen <$> maybeInspection)
  (beeCoveredFramesResult, beeCoveredFramesView) <-
    mreq
      intField
      beeCoveredFramesFs
      (inspectionBeeCoveredFrames <$> maybeInspection)
  (broodFramesResult, broodFramesView) <-
    mreq
      intField
      broodFramesFs
      (inspectionBroodFrames <$> maybeInspection)
  (honeyFramesResult, honeyFramesView) <-
    mreq
      intField
      honeyFramesFs
      (inspectionHoneyFrames <$> maybeInspection)
  (treatmentResult, treatmentView) <-
    mopt
      textField
      treatmentFs
      (inspectionTreatment <$> maybeInspection)
  (feedingResult, feedingView) <-
    mopt
      textField
      feedingFs
      (inspectionFeeding <$> maybeInspection)
  (notesResult, notesView) <-
    mopt
      textareaField
      notesFs
      (inspectionNotes <$> maybeInspection)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (inspectionVersion <$> maybeInspection)
  let vEditInspectionResult = VEditInspection <$> dateResult <*> swarmingTypeIdResult <*> queenSeenResult <*> beeCoveredFramesResult <*> broodFramesResult <*> honeyFramesResult <*> treatmentResult <*> feedingResult <*> notesResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div #dateInputWidget .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label #dateInputLabel .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        <span #dateInputError>
          $maybe err <- fvErrors dateView
            &nbsp;#{err}
    <div #swarmingTypeIdInputWidget .uk-margin-small :not $ null $ fvErrors swarmingTypeIdView:.uk-form-danger>
      <label #swarmingTypeIdInputLabel .uk-form-label :not $ null $ fvErrors swarmingTypeIdView:.uk-text-danger for=#{fvId swarmingTypeIdView}>#{fvLabel swarmingTypeIdView}
      <div .uk-form-controls>
        ^{fvInput swarmingTypeIdView}
        <span #swarmingTypeIdInputError>
          $maybe err <- fvErrors swarmingTypeIdView
            &nbsp;#{err}
    <div #queenSeenInputWidget .uk-margin-small :not $ null $ fvErrors queenSeenView:.uk-form-danger>
      <label #queenSeenInputLabel .uk-form-label :not $ null $ fvErrors queenSeenView:.uk-text-danger for=#{fvId queenSeenView}>#{fvLabel queenSeenView}
      <div .uk-form-controls>
        ^{fvInput queenSeenView}
        <span #queenSeenInputError>
          $maybe err <- fvErrors queenSeenView
            &nbsp;#{err}
    <div #beeCoveredFramesInputWidget .uk-margin-small :not $ null $ fvErrors beeCoveredFramesView:.uk-form-danger>
      <label #beeCoveredFramesInputLabel .uk-form-label :not $ null $ fvErrors beeCoveredFramesView:.uk-text-danger for=#{fvId beeCoveredFramesView}>#{fvLabel beeCoveredFramesView}
      <div .uk-form-controls>
        ^{fvInput beeCoveredFramesView}
        <span #beeCoveredFramesInputError>
          $maybe err <- fvErrors beeCoveredFramesView
            &nbsp;#{err}
    <div #broodFramesInputWidget .uk-margin-small :not $ null $ fvErrors broodFramesView:.uk-form-danger>
      <label #broodFramesInputLabel .uk-form-label :not $ null $ fvErrors broodFramesView:.uk-text-danger for=#{fvId broodFramesView}>#{fvLabel broodFramesView}
      <div .uk-form-controls>
        ^{fvInput broodFramesView}
        <span #broodFramesInputError>
          $maybe err <- fvErrors broodFramesView
            &nbsp;#{err}
    <div #honeyFramesInputWidget .uk-margin-small :not $ null $ fvErrors honeyFramesView:.uk-form-danger>
      <label #honeyFramesInputLabel .uk-form-label :not $ null $ fvErrors honeyFramesView:.uk-text-danger for=#{fvId honeyFramesView}>#{fvLabel honeyFramesView}
      <div .uk-form-controls>
        ^{fvInput honeyFramesView}
        <span #honeyFramesInputError>
          $maybe err <- fvErrors honeyFramesView
            &nbsp;#{err}
    <div #treatmentInputWidget .uk-margin-small :not $ null $ fvErrors treatmentView:.uk-form-danger>
      <label #treatmentInputLabel .uk-form-label :not $ null $ fvErrors treatmentView:.uk-text-danger for=#{fvId treatmentView}>#{fvLabel treatmentView}
      <div .uk-form-controls>
        ^{fvInput treatmentView}
        <span #treatmentInputError>
          $maybe err <- fvErrors treatmentView
            &nbsp;#{err}
    <div #feedingInputWidget .uk-margin-small :not $ null $ fvErrors feedingView:.uk-form-danger>
      <label #feedingInputLabel .uk-form-label :not $ null $ fvErrors feedingView:.uk-text-danger for=#{fvId feedingView}>#{fvLabel feedingView}
      <div .uk-form-controls>
        ^{fvInput feedingView}
        <span #feedingInputError>
          $maybe err <- fvErrors feedingView
            &nbsp;#{err}
    <div #notesInputWidget .uk-margin-small :not $ null $ fvErrors notesView:.uk-form-danger>
      <label #notesInputLabel .uk-form-label :not $ null $ fvErrors notesView:.uk-text-danger for=#{fvId notesView}>#{fvLabel notesView}
      <div .uk-form-controls>
        ^{fvInput notesView}
        <span #notesInputError>
          $maybe err <- fvErrors notesView
            &nbsp;#{err}
    |]
  return (vEditInspectionResult, formWidget)
  where
    dateFs :: FieldSettings App
    dateFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionDate,
          fsTooltip = Nothing,
          fsId = Just "date",
          fsName = Just "date",
          fsAttrs = []
        }
    swarmingTypeIdFs :: FieldSettings App
    swarmingTypeIdFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionSwarmingTypeId,
          fsTooltip = Nothing,
          fsId = Just "swarmingTypeId",
          fsName = Just "swarmingTypeId",
          fsAttrs = []
        }
    queenSeenFs :: FieldSettings App
    queenSeenFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionQueenSeen,
          fsTooltip = Nothing,
          fsId = Just "queenSeen",
          fsName = Just "queenSeen",
          fsAttrs = [("class", "uk-checkbox")]
        }
    beeCoveredFramesFs :: FieldSettings App
    beeCoveredFramesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionBeeCoveredFrames,
          fsTooltip = Nothing,
          fsId = Just "beeCoveredFrames",
          fsName = Just "beeCoveredFrames",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    broodFramesFs :: FieldSettings App
    broodFramesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionBroodFrames,
          fsTooltip = Nothing,
          fsId = Just "broodFrames",
          fsName = Just "broodFrames",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    honeyFramesFs :: FieldSettings App
    honeyFramesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionHoneyFrames,
          fsTooltip = Nothing,
          fsId = Just "honeyFrames",
          fsName = Just "honeyFrames",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }
    treatmentFs :: FieldSettings App
    treatmentFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionTreatment,
          fsTooltip = Nothing,
          fsId = Just "treatment",
          fsName = Just "treatment",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    feedingFs :: FieldSettings App
    feedingFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionFeeding,
          fsTooltip = Nothing,
          fsId = Just "feeding",
          fsName = Just "feeding",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    notesFs :: FieldSettings App
    notesFs =
      FieldSettings
        { fsLabel = SomeMessage MsgInspectionNotes,
          fsTooltip = Nothing,
          fsId = Just "notes",
          fsName = Just "notes",
          fsAttrs = [("class", "uk-textarea uk-form-small uk-width-5-6"), ("rows", "10")]
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
getDeleteInspectionFormR :: InspectionId -> Handler Html
getDeleteInspectionFormR inspectionId = do
  (formWidget, _) <- generateFormPost $ vDeleteInspectionForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalDeleteInspection}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteInspectionR inspectionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

-- gen post delete form - start
postDeleteInspectionR :: InspectionId -> Handler Value
postDeleteInspectionR inspectionId = do
  inspection <- runDB $ get404 inspectionId
  runDB $ delete inspectionId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataR $ inspectionHiveId inspection}

-- gen post delete form - end

-- gen delete form - start
vDeleteInspectionForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteInspectionForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
