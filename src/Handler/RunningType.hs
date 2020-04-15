{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.RunningType where

import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddRunningType = VAddRunningType
  { vAddRunningTypeName :: Text,
    vAddRunningTypeSortIndex :: Int
  }

-- gen data add - end

-- gen get add form - start
getAddRunningTypeFormR :: Handler Html
getAddRunningTypeFormR = do
  (formWidget, _) <- generateFormPost $ vAddRunningTypeForm Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalAddRunningType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddRunningTypeR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get add form - end

-- gen post add form - start
postAddRunningTypeR :: Handler Value
postAddRunningTypeR = do
  ((result, formWidget), _) <- runFormPost $ vAddRunningTypeForm Nothing
  case result of
    FormSuccess vAddRunningType -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let runningType =
            RunningType
              { runningTypeName = vAddRunningTypeName vAddRunningType,
                runningTypeSortIndex = vAddRunningTypeSortIndex vAddRunningType,
                runningTypeVersion = 1,
                runningTypeCreatedAt = curTime,
                runningTypeCreatedBy = userIdent authUser,
                runningTypeUpdatedAt = curTime,
                runningTypeUpdatedBy = userIdent authUser
              }
      runDB $ do
        _ <- insert runningType
        return ()
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post add form - end

-- gen add form - start
vAddRunningTypeForm :: Maybe RunningType -> Html -> MForm Handler (FormResult VAddRunningType, Widget)
vAddRunningTypeForm maybeRunningType extra = do
  (nameResult, nameView) <-
    mreq
      textField
      nameFs
      (runningTypeName <$> maybeRunningType)
  (sortIndexResult, sortIndexView) <-
    mreq
      intField
      sortIndexFs
      (runningTypeSortIndex <$> maybeRunningType)
  let vAddRunningTypeResult = VAddRunningType <$> nameResult <*> sortIndexResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors sortIndexView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors sortIndexView:.uk-text-danger for=#{fvId sortIndexView}>#{fvLabel sortIndexView}
      <div .uk-form-controls>
        ^{fvInput sortIndexView}
        $maybe err <- fvErrors sortIndexView
          &nbsp;#{err}
    |]
  return (vAddRunningTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs =
      FieldSettings
        { fsLabel = SomeMessage MsgRunningTypeName,
          fsTooltip = Nothing,
          fsId = Just "name",
          fsName = Just "name",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    sortIndexFs :: FieldSettings App
    sortIndexFs =
      FieldSettings
        { fsLabel = SomeMessage MsgRunningTypeSortIndex,
          fsTooltip = Nothing,
          fsId = Just "sortIndex",
          fsName = Just "sortIndex",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
        }

-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditRunningType = VEditRunningType
  { vEditRunningTypeName :: Text,
    vEditRunningTypeSortIndex :: Int,
    vEditRunningTypeVersion :: Int
  }

-- gen data edit - end

-- gen get edit form - start
getEditRunningTypeFormR :: RunningTypeId -> Handler Html
getEditRunningTypeFormR runningTypeId = do
  runningType <- runDB $ get404 runningTypeId
  (formWidget, _) <- generateFormPost $ vEditRunningTypeForm (Just runningType)
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalEditRunningType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditRunningTypeR runningTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get edit form - end

-- gen post edit form - start
postEditRunningTypeR :: RunningTypeId -> Handler Value
postEditRunningTypeR runningTypeId = do
  ((result, formWidget), _) <- runFormPost $ vEditRunningTypeForm Nothing
  case result of
    FormSuccess vEditRunningType -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields =
            [ RunningTypeName =. vEditRunningTypeName vEditRunningType,
              RunningTypeSortIndex =. vEditRunningTypeSortIndex vEditRunningType,
              RunningTypeVersion =. vEditRunningTypeVersion vEditRunningType + 1,
              RunningTypeUpdatedAt =. curTime,
              RunningTypeUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <-
          updateWhereCount
            [ RunningTypeId ==. runningTypeId,
              RunningTypeVersion ==. vEditRunningTypeVersion vEditRunningType
            ]
            persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen post edit form - end

-- gen edit form - start
vEditRunningTypeForm :: Maybe RunningType -> Html -> MForm Handler (FormResult VEditRunningType, Widget)
vEditRunningTypeForm maybeRunningType extra = do
  (nameResult, nameView) <-
    mreq
      textField
      nameFs
      (runningTypeName <$> maybeRunningType)
  (sortIndexResult, sortIndexView) <-
    mreq
      intField
      sortIndexFs
      (runningTypeSortIndex <$> maybeRunningType)
  (versionResult, versionView) <-
    mreq
      hiddenField
      versionFs
      (runningTypeVersion <$> maybeRunningType)
  let vEditRunningTypeResult = VEditRunningType <$> nameResult <*> sortIndexResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors nameView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors nameView:.uk-text-danger for=#{fvId nameView}>#{fvLabel nameView}
      <div .uk-form-controls>
        ^{fvInput nameView}
        $maybe err <- fvErrors nameView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors sortIndexView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors sortIndexView:.uk-text-danger for=#{fvId sortIndexView}>#{fvLabel sortIndexView}
      <div .uk-form-controls>
        ^{fvInput sortIndexView}
        $maybe err <- fvErrors sortIndexView
          &nbsp;#{err}
    |]
  return (vEditRunningTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs =
      FieldSettings
        { fsLabel = SomeMessage MsgRunningTypeName,
          fsTooltip = Nothing,
          fsId = Just "name",
          fsName = Just "name",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
    sortIndexFs :: FieldSettings App
    sortIndexFs =
      FieldSettings
        { fsLabel = SomeMessage MsgRunningTypeSortIndex,
          fsTooltip = Nothing,
          fsId = Just "sortIndex",
          fsName = Just "sortIndex",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-medium")]
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
getDeleteRunningTypeFormR :: RunningTypeId -> Handler Html
getDeleteRunningTypeFormR runningTypeId = do
  (formWidget, _) <- generateFormPost $ vDeleteRunningTypeForm
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalDeleteRunningType}
      <form #modal-form .uk-form-horizontal method=post action=@{AdminR $ DeleteRunningTypeR runningTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get delete form - end

-- gen post delete form - start
postDeleteRunningTypeR :: RunningTypeId -> Handler Value
postDeleteRunningTypeR runningTypeId = do
  runDB $ delete runningTypeId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR}

-- gen post delete form - end

-- gen delete form - start
vDeleteRunningTypeForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteRunningTypeForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
