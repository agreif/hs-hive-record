{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Inspectionfile where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add inspectionfile
-------------------------------------------------------

-- gen data add - start
data VAddInspectionfile = VAddInspectionfile
  { vAddInspectionfileFile :: FileInfo
  }
-- gen data add - end

-- gen get add form - start
getAddInspectionfileFormR :: InspectionId -> Handler Html
getAddInspectionfileFormR inspectionId = do
  (formWidget, _) <- generateFormPost $ vAddInspectionfileForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalAddInspectionfile}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddInspectionfileR inspectionId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
      |]
-- gen get add form - end

postAddInspectionfileR :: InspectionId -> Handler Value
postAddInspectionfileR inspectionId = do
  ((result, formWidget), _) <- runFormPost $ vAddInspectionfileForm Nothing
  case result of
    FormSuccess (VAddInspectionfile { vAddInspectionfileFile = fileInfo }) -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      bytes <- fileBytes fileInfo
      inspection <- runDB $ get404 inspectionId
      _ <- runDB $ do
        rawdataId <- insert $ Rawdata
                     { rawdataBytes = bytes
                     , rawdataVersion = 1
                     , rawdataCreatedAt = curTime
                     , rawdataCreatedBy = userIdent authUser
                     , rawdataUpdatedAt = curTime
                     , rawdataUpdatedBy = userIdent authUser
                     }
        insert $ Inspectionfile
          { inspectionfileInspectionId = inspectionId
          , inspectionfileRawdataId = rawdataId
          , inspectionfileFilename = fileName fileInfo
          , inspectionfileMimetype = fileContentType fileInfo
          , inspectionfileSize = length bytes
          , inspectionfileVersion = 1
          , inspectionfileCreatedAt = curTime
          , inspectionfileCreatedBy = userIdent authUser
          , inspectionfileUpdatedAt = curTime
          , inspectionfileUpdatedBy = userIdent authUser
          }
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

getDownloadInspectionfileR :: InspectionfileId -> Handler TypedContent
getDownloadInspectionfileR inspectionfileId = do
  Inspectionfile { inspectionfileFilename = filename
               , inspectionfileMimetype = mimetype
               , inspectionfileSize = size
               , inspectionfileRawdataId = rawdataId
               } <- runDB $ get404 inspectionfileId
  rawdata <- runDB $ get404 rawdataId
  let bytes = rawdataBytes rawdata
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", filename, "\""]
--  addHeader "Content-Length" (pack $ show size)
  sendResponse (TE.encodeUtf8 mimetype, toContent bytes)

vAddInspectionfileForm :: Maybe VAddInspectionfile -> Html -> MForm Handler (FormResult VAddInspectionfile, Widget)
vAddInspectionfileForm maybeVAddInspectionfile extra = do
  (fileResult, fileView) <- mreq fileField
    fileFs
    (vAddInspectionfileFile <$> maybeVAddInspectionfile)
  let vAddInspectionfileResult = VAddInspectionfile <$> fileResult
  let formWidget = toWidget [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vAddInspectionfileResult, formWidget)
  where
    fileFs :: FieldSettings App
    fileFs = FieldSettings
      { fsLabel = "File"
      , fsTooltip = Nothing
      , fsId = Just "file"
      , fsName = Just "file"
      , fsAttrs = []
      }

-------------------------------------------------------
-- edit inspectionfile
-------------------------------------------------------

-- gen data edit - start
data VEditInspectionfile = VEditInspectionfile
  { vEditInspectionfileFile :: FileInfo
  , vEditInspectionfileVersion :: Int
  }
-- gen data edit - end

getEditInspectionfileFormR :: InspectionfileId -> Handler Html
getEditInspectionfileFormR inspectionfileId = do
  inspectionfile <- runDB $ get404 inspectionfileId
  (formWidget, _) <- generateFormPost $ vEditInspectionfileForm inspectionfile Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>Edit Inspection File
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditInspectionfileR inspectionfileId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
|]

postEditInspectionfileR :: InspectionfileId -> Handler Value
postEditInspectionfileR inspectionfileId = do
  inspectionfile <- runDB $ get404 inspectionfileId
  ((result, formWidget), _) <- runFormPost $ vEditInspectionfileForm inspectionfile Nothing
  case result of
    FormSuccess (VEditInspectionfile
                 { vEditInspectionfileFile = fileInfo
                 , vEditInspectionfileVersion = version }) -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      bytes <- fileBytes fileInfo
      inspection <- runDB $ get404 $ inspectionfileInspectionId inspectionfile
      let persistFieldsRawdata =
            [ RawdataBytes =. bytes
            , RawdataVersion =. version + 1
            , RawdataUpdatedAt =. curTime
            , RawdataUpdatedBy =. userIdent authUser
            ]
      let persistFieldsInspectionfile =
            [ InspectionfileFilename =. fileName fileInfo
            , InspectionfileMimetype =. fileContentType fileInfo
            , InspectionfileSize =. length bytes
            , InspectionfileVersion =. version + 1
            , InspectionfileUpdatedAt =. curTime
            , InspectionfileUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        update (inspectionfileRawdataId inspectionfile) persistFieldsRawdata
        updateWhereCount [ InspectionfileId ==. inspectionfileId
                         , InspectionfileVersion ==. version
                         ] persistFieldsInspectionfile
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection}
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

vEditInspectionfileForm :: Inspectionfile -> Maybe VEditInspectionfile -> Html -> MForm Handler (FormResult VEditInspectionfile, Widget)
vEditInspectionfileForm inspectionfile maybeVEditInspectionfile extra = do
  (fileResult, fileView) <- mreq fileField
    fileFs
    (vEditInspectionfileFile <$> maybeVEditInspectionfile)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (Just $ inspectionfileVersion inspectionfile)
  let vEditInspectionfileResult = VEditInspectionfile <$> fileResult <*> versionResult
  let formWidget = toWidget [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vEditInspectionfileResult, formWidget)
  where
    fileFs :: FieldSettings App
    fileFs = FieldSettings
      { fsLabel = "File"
      , fsTooltip = Nothing
      , fsId = Just "file"
      , fsName = Just "file"
      , fsAttrs = []
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

-------------------------------------------------------
-- delete inspectionfile
-------------------------------------------------------

-- gen get delete form - start
getDeleteInspectionfileFormR :: InspectionfileId -> Handler Html
getDeleteInspectionfileFormR inspectionfileId = do
  (formWidget, _) <- generateFormPost $ vDeleteInspectionfileForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalDeleteInspectionfile}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteInspectionfileR inspectionfileId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

postDeleteInspectionfileR :: InspectionfileId -> Handler Value
postDeleteInspectionfileR inspectionfileId = do
  inspectionfile <- runDB $ get404 inspectionfileId
  inspection <- runDB $ get404 $ inspectionfileInspectionId inspectionfile
  runDB $ do
    delete inspectionfileId
    delete $ inspectionfileRawdataId inspectionfile
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ HiveDetailPageDataJsonR $ inspectionHiveId inspection}

-- gen delete form - start
vDeleteInspectionfileForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteInspectionfileForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
