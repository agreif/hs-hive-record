{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Note where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Database.Esqueleto as E
import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Hamlet (hamletFile)

-------------------------------------------------------
-- list
-------------------------------------------------------

getNoteListR :: Handler Html
getNoteListR = do
  let route = HiverecR NoteListDataR
  master <- getYesod
  let isDev = appDev $ appSettings master
  dataUrl <- getUrlRender <*> pure route
  defaultLayout $ toWidget =<< withUrlRenderer $(hamletFile "templates/riot/generic_page.hamlet")

getNoteListDataR :: Handler Value
getNoteListDataR = noteListPageNumDataR 1

postNoteListPageNumDataR :: Int -> Handler Value
postNoteListPageNumDataR pageNum = do
  urlRenderer <- getUrlRender
  returnJson $
    VFormSubmitSuccess
      { fsSuccessDataJsonUrl = urlRenderer $ HiverecR $ NoteListPageNumDataR pageNum
      }

getNoteListPageNumDataR :: Int -> Handler Value
getNoteListPageNumDataR = noteListPageNumDataR

noteListPageNumDataR :: Int -> Handler Value
noteListPageNumDataR pageNum = do
  Entity _ user <- requireAuth
  req <- getRequest
  appName <- runDB configAppName
  urlRenderer <- getUrlRender
  mainNavItems <- mainNavData user MainNavNotes
  (jDataNotes, jDataPaginationItems) <- noteListJDatas pageNum
  let pages =
        defaultDataPages
          { jDataPageNoteList =
              Just $
                JDataPageNoteList
                  { jDataPageNoteListNotes = jDataNotes,
                    jDataPageNoteListAddFormUrl = urlRenderer $ HiverecR AddNoteFormR,
                    jDataPageNoteListPaginationItems = jDataPaginationItems
                  }
          }
  msgHome <- localizedMsg MsgGlobalHome
  msgNote <- localizedMsg MsgNoteNote
  currentLanguage <- getLanguage
  translation <- getTranslation
  let currentDataUrl = urlRenderer $ HiverecR NoteListDataR
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
              { jDataHistoryStateUrl = urlRenderer $ HiverecR NoteListR,
                jDataHistoryStateTitle = msgNote
              },
        jDataCsrfHeaderName = TE.decodeUtf8 $ CI.original defaultCsrfHeaderName,
        jDataCsrfParamName = defaultCsrfParamName,
        jDataCsrfToken = reqToken req,
        jDataBreadcrumbItems =
          [ JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgHome,
                jDataBreadcrumbItemDataUrl = urlRenderer $ HiverecR HomePageDataR
              },
            JDataBreadcrumbItem
              { jDataBreadcrumbItemLabel = msgNote,
                jDataBreadcrumbItemDataUrl = currentDataUrl
              }
          ],
        jDataCurrentLanguage = currentLanguage,
        jDataTranslation = translation,
        jDataLanguageDeUrl = urlRenderer $ HiverecR $ LanguageDeR currentDataUrl,
        jDataLanguageEnUrl = urlRenderer $ HiverecR $ LanguageEnR currentDataUrl
      }

noteListJDatas :: Int -> Handler ([JDataNote], Maybe [JDataPaginationItem])
noteListJDatas pageNum = do
  urlRenderer <- getUrlRender
  rowCount <- runDB $ count ([] :: [Filter Note])
  paginationJDatas <- getPaginationJDatas rowCount noteListPageSize pageNum 11 (HiverecR . NoteListPageNumDataR)
  noteEnts <- runDB loadNoteTuples
  let noteJDatas =
        map
          ( \noteEnt@(Entity noteId _) ->
              JDataNote
                { jDataNoteEnt = noteEnt,
                  jDataNoteEditFormUrl = urlRenderer $ HiverecR $ EditNoteFormR noteId,
                  jDataNoteDeleteFormUrl = urlRenderer $ HiverecR $ DeleteNoteFormR noteId
                }
          )
          noteEnts
  return (noteJDatas, paginationJDatas)
  where
    loadNoteTuples :: YesodDB App [(Entity Note)]
    loadNoteTuples = do
      let pageSize = fromIntegral noteListPageSize
      E.select $ E.from $ \n -> do
        E.offset ((fromIntegral pageNum - 1) * pageSize)
        E.orderBy [E.asc (n E.^. NoteDate)]
        E.limit pageSize
        return n

noteListPageSize :: Int
noteListPageSize = 50

-------------------------------------------------------
-- add
-------------------------------------------------------

defaultAddNote :: Handler (Maybe Note)
defaultAddNote = do
  now <- liftIO getCurrentTime
  today <- liftIO getCurrentDay
  return $ Just $
    Note
      { noteDate = today,
        noteText = Textarea "",
        noteVersion = 1,
        noteCreatedAt = now,
        noteCreatedBy = dbSystemUser,
        noteUpdatedAt = now,
        noteUpdatedBy = dbSystemUser
      }

-- gen data add - start
data VAddNote = VAddNote
  { vAddNoteDate :: Day,
    vAddNoteText :: Textarea
  }

-- gen data add - end

-- gen get add form - start
getAddNoteFormR :: Handler Html
getAddNoteFormR = do
  defaultMaybeAddModel <- defaultAddNote
  (formWidget, _) <- generateFormPost $ vAddNoteForm Nothing defaultMaybeAddModel
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgNoteAddNote}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ AddNoteR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

-- gen post add - start
postAddNoteR :: Handler Value
postAddNoteR = do
  ((result, formWidget), _) <- runFormPost $ vAddNoteForm Nothing Nothing
  case result of
    FormSuccess vAddNote -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let note =
            Note
              { noteDate = vAddNoteDate vAddNote,
                noteText = vAddNoteText vAddNote,
                noteVersion = 1,
                noteCreatedAt = curTime,
                noteCreatedBy = userIdent authUser,
                noteUpdatedAt = curTime,
                noteUpdatedBy = userIdent authUser
              }
      runDB $ do
        _ <- insert note
        return ()
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post add - end

-- gen add form - start
vAddNoteForm :: Maybe NoteId -> Maybe Note -> Html -> MForm Handler (FormResult VAddNote, Widget)
vAddNoteForm maybeNoteId maybeNote extra = do
  (dateResult, dateView) <-
    mreq
      dayField
      dateFs
      (noteDate <$> maybeNote)
  (textResult, textView) <-
    mreq
      textareaField
      textFs
      (noteText <$> maybeNote)
  let vAddNoteResult = VAddNote <$> dateResult <*> textResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #dateInputWidget .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label #dateInputLabel .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        <span #dateInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgNoteDateInputInfo}
        $maybe err <- fvErrors dateView
          <br>
          <span #dateInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #textInputWidget .uk-margin-small :not $ null $ fvErrors textView:.uk-form-danger>
      <label #textInputLabel .uk-form-label :not $ null $ fvErrors textView:.uk-text-danger for=#{fvId textView}>#{fvLabel textView}
      <div .uk-form-controls>
        ^{fvInput textView}
        <span #textInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgNoteTextInputInfo}
        $maybe err <- fvErrors textView
          <br>
          <span #textInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vAddNoteResult, formWidget)
  where
    dateFs :: FieldSettings App
    dateFs =
      FieldSettings
        { fsLabel = SomeMessage MsgNoteDate,
          fsTooltip = Nothing,
          fsId = Just "date",
          fsName = Just "date",
          fsAttrs = []
        }
    textFs :: FieldSettings App
    textFs =
      FieldSettings
        { fsLabel = SomeMessage MsgNoteText,
          fsTooltip = Nothing,
          fsId = Just "text",
          fsName = Just "text",
          fsAttrs = [("class", "uk-textarea uk-form-small uk-width-5-6"), ("rows", "10")]
        }

-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditNote = VEditNote
  { vEditNoteDate :: Day,
    vEditNoteText :: Textarea,
    vEditNoteVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditNoteFormR :: NoteId -> Handler Html
getEditNoteFormR noteId = do
  note <- runDB $ get404 noteId
  (formWidget, _) <- generateFormPost $ vEditNoteForm (Just noteId) (Just note)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgNoteEditNote}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{HiverecR $ EditNoteR noteId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

-- gen post edit - start
postEditNoteR :: NoteId -> Handler Value
postEditNoteR noteId = do
  ((result, formWidget), _) <- runFormPost $ vEditNoteForm (Just noteId) Nothing
  case result of
    FormSuccess vEditNote -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ NoteDate =. vEditNoteDate vEditNote,
              NoteText =. vEditNoteText vEditNote,
              NoteVersion =. vEditNoteVersion vEditNote + 1,
              NoteUpdatedAt =. curTime,
              NoteUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ NoteId ==. noteId,
              NoteVersion ==. vEditNoteVersion vEditNote
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit - end

-- gen edit form - start
vEditNoteForm :: Maybe NoteId -> Maybe Note -> Html -> MForm Handler (FormResult VEditNote, Widget)
vEditNoteForm maybeNoteId maybeNote extra = do
  (dateResult, dateView) <-
    mreq
      dayField
      dateFs
      (noteDate <$> maybeNote)
  (textResult, textView) <-
    mreq
      textareaField
      textFs
      (noteText <$> maybeNote)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (noteVersion <$> maybeNote)
  let vEditNoteResult = VEditNote <$> dateResult <*> textResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div #dateInputWidget .uk-margin-small :not $ null $ fvErrors dateView:.uk-form-danger>
      <label #dateInputLabel .uk-form-label :not $ null $ fvErrors dateView:.uk-text-danger for=#{fvId dateView}>#{fvLabel dateView}
      <div .uk-form-controls>
        ^{fvInput dateView}
        <span #dateInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgNoteDateInputInfo}
        $maybe err <- fvErrors dateView
          <br>
          <span #dateInputError .uk-text-small .input-error>
            &nbsp;#{err}
    <div #textInputWidget .uk-margin-small :not $ null $ fvErrors textView:.uk-form-danger>
      <label #textInputLabel .uk-form-label :not $ null $ fvErrors textView:.uk-text-danger for=#{fvId textView}>#{fvLabel textView}
      <div .uk-form-controls>
        ^{fvInput textView}
        <span #textInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgNoteTextInputInfo}
        $maybe err <- fvErrors textView
          <br>
          <span #textInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vEditNoteResult, formWidget)
  where
    dateFs :: FieldSettings App
    dateFs =
      FieldSettings
        { fsLabel = SomeMessage MsgNoteDate,
          fsTooltip = Nothing,
          fsId = Just "date",
          fsName = Just "date",
          fsAttrs = []
        }
    textFs :: FieldSettings App
    textFs =
      FieldSettings
        { fsLabel = SomeMessage MsgNoteText,
          fsTooltip = Nothing,
          fsId = Just "text",
          fsName = Just "text",
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
getDeleteNoteFormR :: NoteId -> Handler Html
getDeleteNoteFormR noteId = do
  (formWidget, _) <- generateFormPost $ vDeleteNoteForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgNoteDeleteNote}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ DeleteNoteR noteId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

-- gen post delete - start
postDeleteNoteR :: NoteId -> Handler Value
postDeleteNoteR noteId = do
  curTime <- liftIO getCurrentTime
  Entity _ authUser <- requireAuth
  runDB $ do
    -- trick to record the user deleting the entity
    updateWhere
      [NoteId ==. noteId]
      [ NoteUpdatedAt =. curTime,
        NoteUpdatedBy =. userIdent authUser
      ]
    delete noteId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR NoteListDataR}

-- gen post delete - end

-- gen delete form - start
vDeleteNoteForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteNoteForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
