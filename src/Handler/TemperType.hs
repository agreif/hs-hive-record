{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.TemperType where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add
-------------------------------------------------------

-- gen data add - start
data VAddTemperType = VAddTemperType
  { vAddTemperTypeName :: Text
  , vAddTemperTypeSortIndex :: Int
  }
-- gen data add - end

-- gen get add form - start
getAddTemperTypeFormR :: Handler Html
getAddTemperTypeFormR = do
  (formWidget, _) <- generateFormPost $ vAddTemperTypeForm Nothing
  formLayout $
    toWidget [whamlet|
      <h1>_{MsgGlobalAddTemperType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddTemperTypeR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add form - start
postAddTemperTypeR :: Handler Value
postAddTemperTypeR = do
  ((result, formWidget), _) <- runFormPost $ vAddTemperTypeForm Nothing
  case result of
    FormSuccess vAddTemperType -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let temperType = TemperType
            {
            temperTypeName = vAddTemperTypeName vAddTemperType
            , temperTypeSortIndex = vAddTemperTypeSortIndex vAddTemperType
            , temperTypeVersion = 1
            , temperTypeCreatedAt = curTime
            , temperTypeCreatedBy = userIdent authUser
            , temperTypeUpdatedAt = curTime
            , temperTypeUpdatedBy = userIdent authUser
            }
      runDB $ do
        _ <- insert temperType
        return ()
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add form - end

-- gen add form - start
vAddTemperTypeForm :: Maybe TemperType -> Html -> MForm Handler (FormResult VAddTemperType, Widget)
vAddTemperTypeForm maybeTemperType extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (temperTypeName <$> maybeTemperType)
  (sortIndexResult, sortIndexView) <- mreq intField
    sortIndexFs
    (temperTypeSortIndex <$> maybeTemperType)
  let vAddTemperTypeResult = VAddTemperType <$> nameResult <*> sortIndexResult
  let formWidget = toWidget [whamlet|
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
  return (vAddTemperTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgTemperTypeName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sortIndexFs :: FieldSettings App
    sortIndexFs = FieldSettings
      { fsLabel = SomeMessage MsgTemperTypeSortIndex
      , fsTooltip = Nothing
      , fsId = Just "sortIndex"
      , fsName = Just "sortIndex"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
      }
-- gen add form - end

-------------------------------------------------------
-- edit
-------------------------------------------------------

-- gen data edit - start
data VEditTemperType = VEditTemperType
  { vEditTemperTypeName :: Text
  , vEditTemperTypeSortIndex :: Int
  , vEditTemperTypeVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditTemperTypeFormR :: TemperTypeId -> Handler Html
getEditTemperTypeFormR temperTypeId = do
  temperType <- runDB $ get404 temperTypeId
  (formWidget, _) <- generateFormPost $ vEditTemperTypeForm (Just temperType)
  formLayout $
    toWidget [whamlet|
      <h1>_{MsgGlobalEditTemperType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditTemperTypeR temperTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditTemperTypeR :: TemperTypeId -> Handler Value
postEditTemperTypeR temperTypeId = do
  ((result, formWidget), _) <- runFormPost $ vEditTemperTypeForm Nothing
  case result of
    FormSuccess vEditTemperType -> do
      curTime <- liftIO getCurrentTime
      maybeCurRoute <- getCurrentRoute
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            TemperTypeName =. vEditTemperTypeName vEditTemperType
            , TemperTypeSortIndex =. vEditTemperTypeSortIndex vEditTemperType
            , TemperTypeVersion =. vEditTemperTypeVersion vEditTemperType + 1
            , TemperTypeUpdatedAt =. curTime
            , TemperTypeUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ do
        uc <- updateWhereCount [ TemperTypeId ==. temperTypeId
                               , TemperTypeVersion ==. vEditTemperTypeVersion vEditTemperType
                               ] persistFields
        return uc
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditTemperTypeForm :: Maybe TemperType -> Html -> MForm Handler (FormResult VEditTemperType, Widget)
vEditTemperTypeForm maybeTemperType extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (temperTypeName <$> maybeTemperType)
  (sortIndexResult, sortIndexView) <- mreq intField
    sortIndexFs
    (temperTypeSortIndex <$> maybeTemperType)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (temperTypeVersion <$> maybeTemperType)
  let vEditTemperTypeResult = VEditTemperType <$> nameResult <*> sortIndexResult <*> versionResult
  let formWidget = toWidget [whamlet|
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
  return (vEditTemperTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgTemperTypeName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-large") ]
      }
    sortIndexFs :: FieldSettings App
    sortIndexFs = FieldSettings
      { fsLabel = SomeMessage MsgTemperTypeSortIndex
      , fsTooltip = Nothing
      , fsId = Just "sortIndex"
      , fsName = Just "sortIndex"
      , fsAttrs = [ ("class","uk-input uk-form-small uk-form-width-medium") ]
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
getDeleteTemperTypeFormR :: TemperTypeId -> Handler Html
getDeleteTemperTypeFormR temperTypeId = do
  (formWidget, _) <- generateFormPost $ vDeleteTemperTypeForm
  formLayout $
    toWidget [whamlet|
      <h1>_{MsgGlobalDeleteTemperType}
      <form #modal-form .uk-form-horizontal method=post action=@{AdminR $ DeleteTemperTypeR temperTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteTemperTypeR :: TemperTypeId -> Handler Value
postDeleteTemperTypeR temperTypeId = do
  runDB $ delete temperTypeId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
-- gen post delete form - end

-- gen delete form - start
vDeleteTemperTypeForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteTemperTypeForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end
