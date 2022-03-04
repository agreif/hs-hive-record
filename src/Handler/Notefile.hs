{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Notefile where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddNotefile = VAddNotefile
  { vAddNotefileFile :: FileInfo
  }

-- gen data add - end

-- gen get add form - start
getAddNotefileFormR :: NoteId -> Handler Html
getAddNotefileFormR noteId = do
  (formWidget, _) <- generateFormPost $ vAddNotefileForm Nothing Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgNotefileAddNotefile}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddNotefileR noteId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
      |]

-- gen get add form - end

postAddNotefileR :: NoteId -> Handler Value
postAddNotefileR noteId = do
  ((result, formWidget), _) <- runFormPost $ vAddNotefileForm (Just noteId) Nothing
  case result of
    FormSuccess VAddNotefile {vAddNotefileFile = fileInfo} -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      bytes <- fileBytes fileInfo
      note <- runDB $ get404 noteId
      _ <- runDB $ do
        rawdataId <-
          insert $
            Rawdata
              { rawdataBytes = bytes,
                rawdataVersion = 1,
                rawdataCreatedAt = curTime,
                rawdataCreatedBy = userIdent authUser,
                rawdataUpdatedAt = curTime,
                rawdataUpdatedBy = userIdent authUser
              }
        insert $
          Notefile
            { notefileNoteId = noteId,
              notefileRawdataId = rawdataId,
              notefileFilename = fileName fileInfo,
              notefileMimetype = fileContentType fileInfo,
              notefileSize = length bytes,
              notefileVersion = 1,
              notefileCreatedAt = curTime,
              notefileCreatedBy = userIdent authUser,
              notefileUpdatedAt = curTime,
              notefileUpdatedBy = userIdent authUser
            }
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

getDownloadNotefileR :: NotefileId -> Handler TypedContent
getDownloadNotefileR notefileId = do
  Notefile
    { notefileFilename = filename,
      notefileMimetype = mimetype,
      notefileRawdataId = rawdataId
    } <-
    runDB $ get404 notefileId
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", filename, "\""]
  -- rawdata <- runDB $ get404 rawdataId
  -- let bytes = rawdataBytes rawdata
  -- sendResponse (TE.encodeUtf8 mimetype, toContent bytes)

  -- let bytesSource = selectSource [RawdataId ==. rawdataId] []
  -- respondSourceDB (TE.encodeUtf8 mimetype) $ bytesSource $= awaitForever toBuilder'
  -- where
  --   toBuilder' (Entity _ rawdata) = do
  --     sendChunkBS $ rawdataBytes rawdata
  --     sendFlush

  let bytesSource = E.selectSource $ E.from $ \rd -> do
        E.where_ (rd E.^. RawdataId E.==. E.val rawdataId)
        return $ rd E.^. RawdataBytes
  respondSourceDB (TE.encodeUtf8 mimetype) $ bytesSource .| awaitForever toBuilder'
  where
    toBuilder' (E.Value bytes) = do
      sendChunkBS bytes
      sendFlush

vAddNotefileForm :: Maybe NoteId -> Maybe VAddNotefile -> Html -> MForm Handler (FormResult VAddNotefile, Widget)
vAddNotefileForm _ maybeVAddNotefile extra = do
  (fileResult, fileView) <-
    mreq
      fileField
      fileFs
      (vAddNotefileFile <$> maybeVAddNotefile)
  let vAddNotefileResult = VAddNotefile <$> fileResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vAddNotefileResult, formWidget)
  where
    fileFs :: FieldSettings App
    fileFs =
      FieldSettings
        { fsLabel = "File",
          fsTooltip = Nothing,
          fsId = Just "file",
          fsName = Just "file",
          fsAttrs = []
        }

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditNotefile = VEditNotefile
  { vEditNotefileFile :: FileInfo,
    vEditNotefileVersion :: Int
  }

-- gen data edit - end

getEditNotefileFormR :: NotefileId -> Handler Html
getEditNotefileFormR notefileId = do
  notefile <- runDB $ get404 notefileId
  (formWidget, _) <- generateFormPost $ vEditNotefileForm notefile Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>Edit Note File
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditNotefileR notefileId}>
        <div #modal-form-widget>
          ^{formWidget}
      <progress id="modal-form-progressbar" class="uk-progress" value="0" max="0">
|]

postEditNotefileR :: NotefileId -> Handler Value
postEditNotefileR notefileId = do
  notefile <- runDB $ get404 notefileId
  ((result, formWidget), _) <- runFormPost $ vEditNotefileForm notefile Nothing
  case result of
    FormSuccess
      VEditNotefile
        { vEditNotefileFile = fileInfo,
          vEditNotefileVersion = version
        } -> do
        curTime <- liftIO getCurrentTime
        Entity _ authUser <- requireAuth
        urlRenderer <- getUrlRender
        bytes <- fileBytes fileInfo
        note <- runDB $ get404 $ notefileNoteId notefile
        let persistFieldsRawdata =
              [ RawdataBytes =. bytes,
                RawdataVersion =. version + 1,
                RawdataUpdatedAt =. curTime,
                RawdataUpdatedBy =. userIdent authUser
              ]
        let persistFieldsNotefile =
              [ NotefileFilename =. fileName fileInfo,
                NotefileMimetype =. fileContentType fileInfo,
                NotefileSize =. length bytes,
                NotefileVersion =. version + 1,
                NotefileUpdatedAt =. curTime,
                NotefileUpdatedBy =. userIdent authUser
              ]
        updateCount <- runDB $ do
          update (notefileRawdataId notefile) persistFieldsRawdata
          updateWhereCount
            [ NotefileId ==. notefileId,
              NotefileVersion ==. version
            ]
            persistFieldsNotefile
        if updateCount == 1
          then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}
          else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

vEditNotefileForm :: Notefile -> Maybe VEditNotefile -> Html -> MForm Handler (FormResult VEditNotefile, Widget)
vEditNotefileForm notefile maybeVEditNotefile extra = do
  (fileResult, fileView) <-
    mreq
      fileField
      fileFs
      (vEditNotefileFile <$> maybeVEditNotefile)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (Just $ notefileVersion notefile)
  let vEditNotefileResult = VEditNotefile <$> fileResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors fileView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors fileView:.uk-text-danger for=file>File
      <div .uk-form-controls>
        ^{fvInput fileView}
        $maybe err <- fvErrors fileView
          &nbsp;#{err}
    |]
  return (vEditNotefileResult, formWidget)
  where
    fileFs :: FieldSettings App
    fileFs =
      FieldSettings
        { fsLabel = "File",
          fsTooltip = Nothing,
          fsId = Just "file",
          fsName = Just "file",
          fsAttrs = []
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

-------------------------------------------------------
-- delete
-------------------------------------------------------

-- gen get delete form - start
getDeleteNotefileFormR :: NotefileId -> Handler Html
getDeleteNotefileFormR notefileId = do
  (formWidget, _) <- generateFormPost $ vDeleteNotefileForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgNotefileDeleteNotefile}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteNotefileR notefileId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

postDeleteNotefileR :: NotefileId -> Handler Value
postDeleteNotefileR notefileId = do
  notefile <- runDB $ get404 notefileId
  note <- runDB $ get404 $ notefileNoteId notefile
  runDB $ do
    delete notefileId
    delete $ notefileRawdataId notefile
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}

-- gen delete form - start
vDeleteNotefileForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteNotefileForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
