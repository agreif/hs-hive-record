{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Inspection where

import Handler.Common
import Import
--import qualified Database.Esqueleto as E
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add inspection
-------------------------------------------------------


defaultNoteText :: Text
defaultNoteText  = intercalate "\n"
  [ "Wabengassen besetzt:"
  , "Sanftmut: sehr sanft / sanft / nervös / bösartig"
  , "Wabensitz: fest / ruhig / laufend / flüchtig"
  , "Königin:"
  , "Königinen-Zellen:"
  , "Brut: Stifte / offene Brut / verdeckelte Brut"
  , "Schwarmtrieb: fehlt / leicht lenkbar / schlecht lenkbar / sehr stark"
  , "Putztrieb: sehr gut / gut / gering / fehlt"
  , "Winterfestigkeit: gut / mittel / gering / fehlt"
  , "Frühjahrsentwicklung: sehr schnell / schnell / normal / langsam"
  , "Waben +/-:"
  , "Honigwaben entnommen:"
  , "Mittelwände gegeben:"
  , "Anzahl Brutwaben:"
  , "Anzahl Pollenwaben:"
  , "Anzahl Honigwaben:"
  , "Fütterung:"
  , "Wetter:"
  , "Varroamaßnahmen:"
  , "Milbenfall:"
  , "Sonstiges:"
  ]




defaultAddInspection :: HiveId -> Handler Inspection
defaultAddInspection hiveId = do
  now <- liftIO getCurrentTime
  today <- liftIO getCurrentDay
  return $ Inspection
    { inspectionHiveId = hiveId
    , inspectionDate = today
    , inspectionNotes = Textarea defaultNoteText
    , inspectionVersion = 1
    , inspectionCreatedAt = now
    , inspectionCreatedBy = dbSystemUser
    , inspectionUpdatedAt = now
    , inspectionUpdatedBy = dbSystemUser
    }

-- gen data add - start
data VAddInspection = VAddInspection
  { vAddInspectionDate :: Day
  , vAddInspectionNotes :: Textarea
  }
-- gen data add - end

-- gen get add form - start
getAddInspectionFormR :: HiveId -> Handler Html
getAddInspectionFormR hiveId = do
  defaultAddModel <- defaultAddInspection hiveId
  (formWidget, _) <- generateFormPost $ vAddInspectionForm $ Just defaultAddModel
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
            , inspectionNotes = vAddInspectionNotes vAddInspection
            , inspectionVersion = 1
            , inspectionCreatedAt = curTime
            , inspectionCreatedBy = userIdent authUser
            , inspectionUpdatedAt = curTime
            , inspectionUpdatedBy = userIdent authUser
            }
      _ <- runDB $ insert inspection
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
  (notesResult, notesView) <- mreq textareaField
    notesFs
    (inspectionNotes <$> maybeInspection)
  let vAddInspectionResult = VAddInspection <$> dateResult <*> notesResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        $maybe err <- fvErrors dateView
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
      { fsLabel = SomeMessage MsgAddInspectionDate
      , fsTooltip = Nothing
      , fsId = Just "date"
      , fsName = Just "date"
      , fsAttrs = [  ]
      }
    notesFs :: FieldSettings App
    notesFs = FieldSettings
      { fsLabel = SomeMessage MsgAddInspectionNotes
      , fsTooltip = Nothing
      , fsId = Just "notes"
      , fsName = Just "notes"
      , fsAttrs = [ ("class","uk-form-width-large uk-textarea uk-form-small uk-width-5-6"), ("rows","27") ]
      }

data MsgAddInspection =
  MsgAddInspectionDate
  | MsgAddInspectionNotes

instance RenderMessage App MsgAddInspection where
  renderMessage _ []        = renderAddInspectionGerman
  renderMessage _ ("de":_) = renderAddInspectionGerman
  renderMessage _ ("en":_) = renderAddInspectionEnglish
  renderMessage _ ("en-US":_) = renderAddInspectionEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderAddInspectionGerman :: MsgAddInspection -> Text
renderAddInspectionGerman MsgAddInspectionDate = "Datum"
renderAddInspectionGerman MsgAddInspectionNotes = "Notizen"


renderAddInspectionEnglish :: MsgAddInspection -> Text
renderAddInspectionEnglish MsgAddInspectionDate = "Date"
renderAddInspectionEnglish MsgAddInspectionNotes = "Notes"

-- gen add form - end

-------------------------------------------------------
-- edit inspection
-------------------------------------------------------

-- gen data edit - start
data VEditInspection = VEditInspection
  { vEditInspectionDate :: Day
  , vEditInspectionNotes :: Textarea
  , vEditInspectionVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditInspectionFormR :: InspectionId -> Handler Html
getEditInspectionFormR inspectionId = do
  inspection <- runDB $ get404 inspectionId
  (formWidget, _) <- generateFormPost $ vEditInspectionForm $ Just inspection
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
            , InspectionNotes =. vEditInspectionNotes vEditInspection
            , InspectionVersion =. vEditInspectionVersion vEditInspection + 1
            , InspectionUpdatedAt =. curTime
            , InspectionUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ updateWhereCount [ InspectionId ==. inspectionId
                                              , InspectionVersion ==. vEditInspectionVersion vEditInspection
                                              ] persistFields
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
  (notesResult, notesView) <- mreq textareaField
    notesFs
    (inspectionNotes <$> maybeInspection)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (inspectionVersion <$> maybeInspection)
  let vEditInspectionResult = VEditInspection <$> dateResult <*> notesResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        $maybe err <- fvErrors dateView
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
      { fsLabel = SomeMessage MsgEditInspectionDate
      , fsTooltip = Nothing
      , fsId = Just "date"
      , fsName = Just "date"
      , fsAttrs = [  ]
      }
    notesFs :: FieldSettings App
    notesFs = FieldSettings
      { fsLabel = SomeMessage MsgEditInspectionNotes
      , fsTooltip = Nothing
      , fsId = Just "notes"
      , fsName = Just "notes"
      , fsAttrs = [ ("class","uk-form-width-large uk-textarea uk-form-small uk-width-5-6"), ("rows","27") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

data MsgEditInspection =
  MsgEditInspectionDate
  | MsgEditInspectionNotes

instance RenderMessage App MsgEditInspection where
  renderMessage _ []        = renderEditInspectionGerman
  renderMessage _ ("de":_) = renderEditInspectionGerman
  renderMessage _ ("en":_) = renderEditInspectionEnglish
  renderMessage _ ("en-US":_) = renderEditInspectionEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderEditInspectionGerman :: MsgEditInspection -> Text
renderEditInspectionGerman MsgEditInspectionDate = "Datum"
renderEditInspectionGerman MsgEditInspectionNotes = "Notizen"


renderEditInspectionEnglish :: MsgEditInspection -> Text
renderEditInspectionEnglish MsgEditInspectionDate = "Date"
renderEditInspectionEnglish MsgEditInspectionNotes = "Notes"

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
