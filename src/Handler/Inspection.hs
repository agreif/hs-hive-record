{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Inspection where

import Handler.Common
import Import
import qualified Data.List as L
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

temperTypeSelectField :: Field Handler (Key TemperType)
temperTypeSelectField = do
  selectField $ optionsPersistKey [] [Asc TemperTypeSortIndex] temperTypeName

runningTypeSelectField :: Field Handler (Key RunningType)
runningTypeSelectField = do
  selectField $ optionsPersistKey [] [Asc RunningTypeSortIndex] runningTypeName

swarmingTypeSelectField :: Field Handler (Key SwarmingType)
swarmingTypeSelectField = do
  selectField $ optionsPersistKey [] [Asc SwarmingTypeSortIndex] swarmingTypeName

-------------------------------------------------------
-- add inspection
-------------------------------------------------------

defaultAddInspection :: HiveId -> Handler Inspection
defaultAddInspection hiveId = do
  now <- liftIO getCurrentTime
  today <- liftIO getCurrentDay
  -- assume there any types
  temperTypeId <- runDB defaultTemperTypeId
  runningTypeId <- runDB defaultRunningTypeId
  swarmingTypeId <- runDB defaultSwarmingTypeId
  maybeLastInspectionEnt <- runDB $ getLastInspectionEnt hiveId
  case maybeLastInspectionEnt of
    Just (Entity _ inspection) ->
      return $ Inspection
      { inspectionHiveId = hiveId
      , inspectionDate = today
      , inspectionTemperTypeId = inspectionTemperTypeId inspection
      , inspectionRunningTypeId = inspectionRunningTypeId inspection
      , inspectionSwarmingTypeId = inspectionSwarmingTypeId inspection
      , inspectionQueenSeen = False
      , inspectionBeeCoveredFrames = inspectionBeeCoveredFrames inspection
      , inspectionTotalFrames = inspectionTotalFrames inspection
      , inspectionBroodFrames = inspectionBroodFrames inspection
      , inspectionHoneyFrames = inspectionHoneyFrames inspection
      , inspectionTreatment = Nothing
      , inspectionFeeding = Nothing
      , inspectionNotes = Nothing
      , inspectionVersion = 1
      , inspectionCreatedAt = now
      , inspectionCreatedBy = dbSystemUser
      , inspectionUpdatedAt = now
      , inspectionUpdatedBy = dbSystemUser
      }
    _ ->
      return $ Inspection
      { inspectionHiveId = hiveId
      , inspectionDate = today
      , inspectionTemperTypeId = temperTypeId
      , inspectionRunningTypeId = runningTypeId
      , inspectionSwarmingTypeId = swarmingTypeId
      , inspectionQueenSeen = False
      , inspectionBeeCoveredFrames = 0
      , inspectionTotalFrames = 0
      , inspectionBroodFrames = 0
      , inspectionHoneyFrames = 0
      , inspectionTreatment = Nothing
      , inspectionFeeding = Nothing
      , inspectionNotes = Nothing
      , inspectionVersion = 1
      , inspectionCreatedAt = now
      , inspectionCreatedBy = dbSystemUser
      , inspectionUpdatedAt = now
      , inspectionUpdatedBy = dbSystemUser
      }
  where
    defaultTemperTypeId :: YesodDB App TemperTypeId
    defaultTemperTypeId = do
      temperTypeEnts <- selectList ([] :: [Filter TemperType]) []
      return $ snd $ L.head $ L.sort $ L.map (\(Entity ttId tt) -> (temperTypeSortIndex tt, ttId)) temperTypeEnts
    defaultRunningTypeId :: YesodDB App RunningTypeId
    defaultRunningTypeId = do
      runningTypeEnts <- selectList ([] :: [Filter RunningType]) []
      return $ snd $ L.head $ L.sort $ L.map (\(Entity rtId rt) -> (runningTypeSortIndex rt, rtId)) runningTypeEnts
    defaultSwarmingTypeId :: YesodDB App SwarmingTypeId
    defaultSwarmingTypeId = do
      swarmingTypeEnts <- selectList ([] :: [Filter SwarmingType]) []
      return $ snd $ L.head $ L.sort $ L.map (\(Entity stId st) -> (swarmingTypeSortIndex st, stId)) swarmingTypeEnts

-- gen data add - start
data VAddInspection = VAddInspection
  { vAddInspectionDate :: Day
  , vAddInspectionTemperTypeId :: TemperTypeId
  , vAddInspectionRunningTypeId :: RunningTypeId
  , vAddInspectionSwarmingTypeId :: SwarmingTypeId
  , vAddInspectionQueenSeen :: Bool
  , vAddInspectionTotalFrames :: Int
  , vAddInspectionBeeCoveredFrames :: Int
  , vAddInspectionBroodFrames :: Int
  , vAddInspectionHoneyFrames :: Int
  , vAddInspectionTreatment :: Maybe Text
  , vAddInspectionFeeding :: Maybe Text
  , vAddInspectionNotes :: Maybe Textarea
  }
-- gen data add - end

-- gen get add form - start
getAddInspectionFormR :: HiveId -> Handler Html
getAddInspectionFormR hiveId = do
  defaultMaybeAddModel <- defaultAddInspection hiveId
  (formWidget, _) <- generateFormPost $ vAddInspectionForm defaultMaybeAddModel
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalAddInspection}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddInspectionR hiveId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add form - start
postAddInspectionR :: HiveId -> Handler Value
postAddInspectionR hiveId = do
  ((result, formWidget), _) <- runFormPost $ vAddInspectionForm Nothing
  case result of
    FormSuccess vAddInspection -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let inspection = Inspection
            {
            inspectionHiveId = hiveId
            , inspectionDate = vAddInspectionDate vAddInspection
            , inspectionTemperTypeId = vAddInspectionTemperTypeId vAddInspection
            , inspectionRunningTypeId = vAddInspectionRunningTypeId vAddInspection
            , inspectionSwarmingTypeId = vAddInspectionSwarmingTypeId vAddInspection
            , inspectionQueenSeen = vAddInspectionQueenSeen vAddInspection
            , inspectionTotalFrames = vAddInspectionTotalFrames vAddInspection
            , inspectionBeeCoveredFrames = vAddInspectionBeeCoveredFrames vAddInspection
            , inspectionBroodFrames = vAddInspectionBroodFrames vAddInspection
            , inspectionHoneyFrames = vAddInspectionHoneyFrames vAddInspection
            , inspectionTreatment = vAddInspectionTreatment vAddInspection
            , inspectionFeeding = vAddInspectionFeeding vAddInspection
            , inspectionNotes = vAddInspectionNotes vAddInspection
            , inspectionVersion = 1
            , inspectionCreatedAt = curTime
            , inspectionCreatedBy = userIdent authUser
            , inspectionUpdatedAt = curTime
            , inspectionUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert inspection
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add form - end

-- gen add form - start
vAddInspectionForm :: Maybe Inspection -> Html -> MForm Handler (FormResult VAddInspection, Widget)
vAddInspectionForm maybeInspection extra = do
  (dateResult, dateView) <- mreq dayField
    dateFs
    (inspectionDate <$> maybeInspection)
  (temperTypeIdResult, temperTypeIdView) <- mreq temperTypeSelectField
    temperTypeIdFs
    (inspectionTemperTypeId <$> maybeInspection)
  (runningTypeIdResult, runningTypeIdView) <- mreq runningTypeSelectField
    runningTypeIdFs
    (inspectionRunningTypeId <$> maybeInspection)
  (swarmingTypeIdResult, swarmingTypeIdView) <- mreq swarmingTypeSelectField
    swarmingTypeIdFs
    (inspectionSwarmingTypeId <$> maybeInspection)
  (queenSeenResult, queenSeenView) <- mreq checkBoxField
    queenSeenFs
    (inspectionQueenSeen <$> maybeInspection)
  (totalFramesResult, totalFramesView) <- mreq intField
    totalFramesFs
    (inspectionTotalFrames <$> maybeInspection)
  (beeCoveredFramesResult, beeCoveredFramesView) <- mreq intField
    beeCoveredFramesFs
    (inspectionBeeCoveredFrames <$> maybeInspection)
  (broodFramesResult, broodFramesView) <- mreq intField
    broodFramesFs
    (inspectionBroodFrames <$> maybeInspection)
  (honeyFramesResult, honeyFramesView) <- mreq intField
    honeyFramesFs
    (inspectionHoneyFrames <$> maybeInspection)
  (treatmentResult, treatmentView) <- mopt textField
    treatmentFs
    (inspectionTreatment <$> maybeInspection)
  (feedingResult, feedingView) <- mopt textField
    feedingFs
    (inspectionFeeding <$> maybeInspection)
  (notesResult, notesView) <- mopt textareaField
    notesFs
    (inspectionNotes <$> maybeInspection)
  let vAddInspectionResult = VAddInspection <$> dateResult <*> temperTypeIdResult <*> runningTypeIdResult <*> swarmingTypeIdResult <*> queenSeenResult <*> totalFramesResult <*> beeCoveredFramesResult <*> broodFramesResult <*> honeyFramesResult <*> treatmentResult <*> feedingResult <*> notesResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        $maybe err <- fvErrors dateView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors temperTypeIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors temperTypeIdView:.uk-text-danger for=#{fvId temperTypeIdView}>#{fvLabel temperTypeIdView}
      <div .uk-form-controls>
        ^{fvInput temperTypeIdView}
        $maybe err <- fvErrors temperTypeIdView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors runningTypeIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors runningTypeIdView:.uk-text-danger for=#{fvId runningTypeIdView}>#{fvLabel runningTypeIdView}
      <div .uk-form-controls>
        ^{fvInput runningTypeIdView}
        $maybe err <- fvErrors runningTypeIdView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors swarmingTypeIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors swarmingTypeIdView:.uk-text-danger for=#{fvId swarmingTypeIdView}>#{fvLabel swarmingTypeIdView}
      <div .uk-form-controls>
        ^{fvInput swarmingTypeIdView}
        $maybe err <- fvErrors swarmingTypeIdView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors queenSeenView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors queenSeenView:.uk-text-danger for=#{fvId queenSeenView}>#{fvLabel queenSeenView}
      <div .uk-form-controls>
        ^{fvInput queenSeenView}
        $maybe err <- fvErrors queenSeenView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors totalFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors totalFramesView:.uk-text-danger for=#{fvId totalFramesView}>#{fvLabel totalFramesView}
      <div .uk-form-controls>
        ^{fvInput totalFramesView}
        $maybe err <- fvErrors totalFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors beeCoveredFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors beeCoveredFramesView:.uk-text-danger for=#{fvId beeCoveredFramesView}>#{fvLabel beeCoveredFramesView}
      <div .uk-form-controls>
        ^{fvInput beeCoveredFramesView}
        $maybe err <- fvErrors beeCoveredFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors broodFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors broodFramesView:.uk-text-danger for=#{fvId broodFramesView}>#{fvLabel broodFramesView}
      <div .uk-form-controls>
        ^{fvInput broodFramesView}
        $maybe err <- fvErrors broodFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors honeyFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors honeyFramesView:.uk-text-danger for=#{fvId honeyFramesView}>#{fvLabel honeyFramesView}
      <div .uk-form-controls>
        ^{fvInput honeyFramesView}
        $maybe err <- fvErrors honeyFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors treatmentView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors treatmentView:.uk-text-danger for=#{fvId treatmentView}>#{fvLabel treatmentView}
      <div .uk-form-controls>
        ^{fvInput treatmentView}
        $maybe err <- fvErrors treatmentView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors feedingView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors feedingView:.uk-text-danger for=#{fvId feedingView}>#{fvLabel feedingView}
      <div .uk-form-controls>
        ^{fvInput feedingView}
        $maybe err <- fvErrors feedingView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors notesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors notesView:.uk-text-danger for=#{fvId notesView}>#{fvLabel notesView}
      <div .uk-form-controls>
        ^{fvInput notesView}
        $maybe err <- fvErrors notesView
          &nbsp;#{err}
    |]
  return (vAddInspectionResult, formWidget)
  where
    dateFs :: FieldSettings App
    dateFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionDate
      , fsTooltip = Nothing
      , fsId = Just "date"
      , fsName = Just "date"
      , fsAttrs = [  ]
      }
    temperTypeIdFs :: FieldSettings App
    temperTypeIdFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionTemperTypeId
      , fsTooltip = Nothing
      , fsId = Just "temperTypeId"
      , fsName = Just "temperTypeId"
      , fsAttrs = [  ]
      }
    runningTypeIdFs :: FieldSettings App
    runningTypeIdFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionRunningTypeId
      , fsTooltip = Nothing
      , fsId = Just "runningTypeId"
      , fsName = Just "runningTypeId"
      , fsAttrs = [  ]
      }
    swarmingTypeIdFs :: FieldSettings App
    swarmingTypeIdFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionSwarmingTypeId
      , fsTooltip = Nothing
      , fsId = Just "swarmingTypeId"
      , fsName = Just "swarmingTypeId"
      , fsAttrs = [  ]
      }
    queenSeenFs :: FieldSettings App
    queenSeenFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionQueenSeen
      , fsTooltip = Nothing
      , fsId = Just "queenSeen"
      , fsName = Just "queenSeen"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    totalFramesFs :: FieldSettings App
    totalFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionTotalFrames
      , fsTooltip = Nothing
      , fsId = Just "totalFrames"
      , fsName = Just "totalFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    beeCoveredFramesFs :: FieldSettings App
    beeCoveredFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionBeeCoveredFrames
      , fsTooltip = Nothing
      , fsId = Just "beeCoveredFrames"
      , fsName = Just "beeCoveredFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    broodFramesFs :: FieldSettings App
    broodFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionBroodFrames
      , fsTooltip = Nothing
      , fsId = Just "broodFrames"
      , fsName = Just "broodFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    honeyFramesFs :: FieldSettings App
    honeyFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionHoneyFrames
      , fsTooltip = Nothing
      , fsId = Just "honeyFrames"
      , fsName = Just "honeyFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    treatmentFs :: FieldSettings App
    treatmentFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionTreatment
      , fsTooltip = Nothing
      , fsId = Just "treatment"
      , fsName = Just "treatment"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    feedingFs :: FieldSettings App
    feedingFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionFeeding
      , fsTooltip = Nothing
      , fsId = Just "feeding"
      , fsName = Just "feeding"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    notesFs :: FieldSettings App
    notesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionNotes
      , fsTooltip = Nothing
      , fsId = Just "notes"
      , fsName = Just "notes"
      , fsAttrs = [ ("class","uk-textarea uk-form-small uk-width-5-6"), ("rows","10") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit inspection
-------------------------------------------------------

-- gen data edit - start
data VEditInspection = VEditInspection
  { vEditInspectionDate :: Day
  , vEditInspectionTemperTypeId :: TemperTypeId
  , vEditInspectionRunningTypeId :: RunningTypeId
  , vEditInspectionSwarmingTypeId :: SwarmingTypeId
  , vEditInspectionQueenSeen :: Bool
  , vEditInspectionTotalFrames :: Int
  , vEditInspectionBeeCoveredFrames :: Int
  , vEditInspectionBroodFrames :: Int
  , vEditInspectionHoneyFrames :: Int
  , vEditInspectionTreatment :: Maybe Text
  , vEditInspectionFeeding :: Maybe Text
  , vEditInspectionNotes :: Maybe Textarea
  , vEditInspectionVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditInspectionFormR :: InspectionId -> Handler Html
getEditInspectionFormR inspectionId = do
  inspection <- runDB $ get404 inspectionId
  (formWidget, _) <- generateFormPost $ vEditInspectionForm (Just inspection)
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalEditInspection}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditInspectionR inspectionId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditInspectionR :: InspectionId -> Handler Value
postEditInspectionR inspectionId = do
  ((result, formWidget), _) <- runFormPost $ vEditInspectionForm Nothing
  case result of
    FormSuccess vEditInspection -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      inspection <- runDB $ get404 inspectionId
      let persistFields = [
            InspectionDate =. vEditInspectionDate vEditInspection
            , InspectionTemperTypeId =. vEditInspectionTemperTypeId vEditInspection
            , InspectionRunningTypeId =. vEditInspectionRunningTypeId vEditInspection
            , InspectionSwarmingTypeId =. vEditInspectionSwarmingTypeId vEditInspection
            , InspectionQueenSeen =. vEditInspectionQueenSeen vEditInspection
            , InspectionTotalFrames =. vEditInspectionTotalFrames vEditInspection
            , InspectionBeeCoveredFrames =. vEditInspectionBeeCoveredFrames vEditInspection
            , InspectionBroodFrames =. vEditInspectionBroodFrames vEditInspection
            , InspectionHoneyFrames =. vEditInspectionHoneyFrames vEditInspection
            , InspectionTreatment =. vEditInspectionTreatment vEditInspection
            , InspectionFeeding =. vEditInspectionFeeding vEditInspection
            , InspectionNotes =. vEditInspectionNotes vEditInspection
            , InspectionVersion =. vEditInspectionVersion vEditInspection + 1
            , InspectionUpdatedAt =. curTime
            , InspectionUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ InspectionId ==. inspectionId
                               , InspectionVersion ==. vEditInspectionVersion vEditInspection
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditInspectionForm :: Maybe Inspection -> Html -> MForm Handler (FormResult VEditInspection, Widget)
vEditInspectionForm maybeInspection extra = do
  (dateResult, dateView) <- mreq dayField
    dateFs
    (inspectionDate <$> maybeInspection)
  (temperTypeIdResult, temperTypeIdView) <- mreq temperTypeSelectField
    temperTypeIdFs
    (inspectionTemperTypeId <$> maybeInspection)
  (runningTypeIdResult, runningTypeIdView) <- mreq runningTypeSelectField
    runningTypeIdFs
    (inspectionRunningTypeId <$> maybeInspection)
  (swarmingTypeIdResult, swarmingTypeIdView) <- mreq swarmingTypeSelectField
    swarmingTypeIdFs
    (inspectionSwarmingTypeId <$> maybeInspection)
  (queenSeenResult, queenSeenView) <- mreq checkBoxField
    queenSeenFs
    (inspectionQueenSeen <$> maybeInspection)
  (totalFramesResult, totalFramesView) <- mreq intField
    totalFramesFs
    (inspectionTotalFrames <$> maybeInspection)
  (beeCoveredFramesResult, beeCoveredFramesView) <- mreq intField
    beeCoveredFramesFs
    (inspectionBeeCoveredFrames <$> maybeInspection)
  (broodFramesResult, broodFramesView) <- mreq intField
    broodFramesFs
    (inspectionBroodFrames <$> maybeInspection)
  (honeyFramesResult, honeyFramesView) <- mreq intField
    honeyFramesFs
    (inspectionHoneyFrames <$> maybeInspection)
  (treatmentResult, treatmentView) <- mopt textField
    treatmentFs
    (inspectionTreatment <$> maybeInspection)
  (feedingResult, feedingView) <- mopt textField
    feedingFs
    (inspectionFeeding <$> maybeInspection)
  (notesResult, notesView) <- mopt textareaField
    notesFs
    (inspectionNotes <$> maybeInspection)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (inspectionVersion <$> maybeInspection)
  let vEditInspectionResult = VEditInspection <$> dateResult <*> temperTypeIdResult <*> runningTypeIdResult <*> swarmingTypeIdResult <*> queenSeenResult <*> totalFramesResult <*> beeCoveredFramesResult <*> broodFramesResult <*> honeyFramesResult <*> treatmentResult <*> feedingResult <*> notesResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        $maybe err <- fvErrors dateView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors temperTypeIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors temperTypeIdView:.uk-text-danger for=#{fvId temperTypeIdView}>#{fvLabel temperTypeIdView}
      <div .uk-form-controls>
        ^{fvInput temperTypeIdView}
        $maybe err <- fvErrors temperTypeIdView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors runningTypeIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors runningTypeIdView:.uk-text-danger for=#{fvId runningTypeIdView}>#{fvLabel runningTypeIdView}
      <div .uk-form-controls>
        ^{fvInput runningTypeIdView}
        $maybe err <- fvErrors runningTypeIdView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors swarmingTypeIdView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors swarmingTypeIdView:.uk-text-danger for=#{fvId swarmingTypeIdView}>#{fvLabel swarmingTypeIdView}
      <div .uk-form-controls>
        ^{fvInput swarmingTypeIdView}
        $maybe err <- fvErrors swarmingTypeIdView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors queenSeenView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors queenSeenView:.uk-text-danger for=#{fvId queenSeenView}>#{fvLabel queenSeenView}
      <div .uk-form-controls>
        ^{fvInput queenSeenView}
        $maybe err <- fvErrors queenSeenView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors totalFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors totalFramesView:.uk-text-danger for=#{fvId totalFramesView}>#{fvLabel totalFramesView}
      <div .uk-form-controls>
        ^{fvInput totalFramesView}
        $maybe err <- fvErrors totalFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors beeCoveredFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors beeCoveredFramesView:.uk-text-danger for=#{fvId beeCoveredFramesView}>#{fvLabel beeCoveredFramesView}
      <div .uk-form-controls>
        ^{fvInput beeCoveredFramesView}
        $maybe err <- fvErrors beeCoveredFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors broodFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors broodFramesView:.uk-text-danger for=#{fvId broodFramesView}>#{fvLabel broodFramesView}
      <div .uk-form-controls>
        ^{fvInput broodFramesView}
        $maybe err <- fvErrors broodFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors honeyFramesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors honeyFramesView:.uk-text-danger for=#{fvId honeyFramesView}>#{fvLabel honeyFramesView}
      <div .uk-form-controls>
        ^{fvInput honeyFramesView}
        $maybe err <- fvErrors honeyFramesView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors treatmentView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors treatmentView:.uk-text-danger for=#{fvId treatmentView}>#{fvLabel treatmentView}
      <div .uk-form-controls>
        ^{fvInput treatmentView}
        $maybe err <- fvErrors treatmentView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors feedingView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors feedingView:.uk-text-danger for=#{fvId feedingView}>#{fvLabel feedingView}
      <div .uk-form-controls>
        ^{fvInput feedingView}
        $maybe err <- fvErrors feedingView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors notesView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors notesView:.uk-text-danger for=#{fvId notesView}>#{fvLabel notesView}
      <div .uk-form-controls>
        ^{fvInput notesView}
        $maybe err <- fvErrors notesView
          &nbsp;#{err}
    |]
  return (vEditInspectionResult, formWidget)
  where
    dateFs :: FieldSettings App
    dateFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionDate
      , fsTooltip = Nothing
      , fsId = Just "date"
      , fsName = Just "date"
      , fsAttrs = [  ]
      }
    temperTypeIdFs :: FieldSettings App
    temperTypeIdFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionTemperTypeId
      , fsTooltip = Nothing
      , fsId = Just "temperTypeId"
      , fsName = Just "temperTypeId"
      , fsAttrs = [  ]
      }
    runningTypeIdFs :: FieldSettings App
    runningTypeIdFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionRunningTypeId
      , fsTooltip = Nothing
      , fsId = Just "runningTypeId"
      , fsName = Just "runningTypeId"
      , fsAttrs = [  ]
      }
    swarmingTypeIdFs :: FieldSettings App
    swarmingTypeIdFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionSwarmingTypeId
      , fsTooltip = Nothing
      , fsId = Just "swarmingTypeId"
      , fsName = Just "swarmingTypeId"
      , fsAttrs = [  ]
      }
    queenSeenFs :: FieldSettings App
    queenSeenFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionQueenSeen
      , fsTooltip = Nothing
      , fsId = Just "queenSeen"
      , fsName = Just "queenSeen"
      , fsAttrs = [ ("class","uk-checkbox") ]
      }
    totalFramesFs :: FieldSettings App
    totalFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionTotalFrames
      , fsTooltip = Nothing
      , fsId = Just "totalFrames"
      , fsName = Just "totalFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    beeCoveredFramesFs :: FieldSettings App
    beeCoveredFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionBeeCoveredFrames
      , fsTooltip = Nothing
      , fsId = Just "beeCoveredFrames"
      , fsName = Just "beeCoveredFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    broodFramesFs :: FieldSettings App
    broodFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionBroodFrames
      , fsTooltip = Nothing
      , fsId = Just "broodFrames"
      , fsName = Just "broodFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    honeyFramesFs :: FieldSettings App
    honeyFramesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionHoneyFrames
      , fsTooltip = Nothing
      , fsId = Just "honeyFrames"
      , fsName = Just "honeyFrames"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
    treatmentFs :: FieldSettings App
    treatmentFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionTreatment
      , fsTooltip = Nothing
      , fsId = Just "treatment"
      , fsName = Just "treatment"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    feedingFs :: FieldSettings App
    feedingFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionFeeding
      , fsTooltip = Nothing
      , fsId = Just "feeding"
      , fsName = Just "feeding"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    notesFs :: FieldSettings App
    notesFs = FieldSettings
      { fsLabel = SomeMessage MsgInspectionNotes
      , fsTooltip = Nothing
      , fsId = Just "notes"
      , fsName = Just "notes"
      , fsAttrs = [ ("class","uk-textarea uk-form-small uk-width-5-6"), ("rows","10") ]
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
-- delete inspection
-------------------------------------------------------

-- gen get delete form - start
getDeleteInspectionFormR :: InspectionId -> Handler Html
getDeleteInspectionFormR inspectionId = do
  (formWidget, _) <- generateFormPost $ vDeleteInspectionForm
  formLayout $ do
    toWidget [whamlet|
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
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection }
-- gen post delete form - end

-- gen delete form - start
vDeleteInspectionForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteInspectionForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
