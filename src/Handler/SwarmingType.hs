{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SwarmingType where

import Handler.Common
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Database.Persist.Sql (updateWhereCount)

-------------------------------------------------------
-- add swarming type
-------------------------------------------------------

-- gen data add - start
data VAddSwarmingType = VAddSwarmingType
  { vAddSwarmingTypeName :: Text
  , vAddSwarmingTypeSortIndex :: Int
  }
-- gen data add - end

-- gen get add form - start
getAddSwarmingTypeFormR :: Handler Html
getAddSwarmingTypeFormR = do
  (formWidget, _) <- generateFormPost $ vAddSwarmingTypeForm Nothing
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalAddSwarmingType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ AddSwarmingTypeR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get add form - end

-- gen post add form - start
postAddSwarmingTypeR :: Handler Value
postAddSwarmingTypeR = do
  ((result, formWidget), _) <- runFormPost $ vAddSwarmingTypeForm Nothing
  case result of
    FormSuccess vAddSwarmingType -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let swarmingType = SwarmingType
            {
            swarmingTypeName = vAddSwarmingTypeName vAddSwarmingType
            , swarmingTypeSortIndex = vAddSwarmingTypeSortIndex vAddSwarmingType
            , swarmingTypeVersion = 1
            , swarmingTypeCreatedAt = curTime
            , swarmingTypeCreatedBy = userIdent authUser
            , swarmingTypeUpdatedAt = curTime
            , swarmingTypeUpdatedBy = userIdent authUser
            }
      _ <- runDB $ insert swarmingType
      returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }
-- gen post add form - end

-- gen add form - start
vAddSwarmingTypeForm :: Maybe SwarmingType -> Html -> MForm Handler (FormResult VAddSwarmingType, Widget)
vAddSwarmingTypeForm maybeSwarmingType extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (swarmingTypeName <$> maybeSwarmingType)
  (sortIndexResult, sortIndexView) <- mreq intField
    sortIndexFs
    (swarmingTypeSortIndex <$> maybeSwarmingType)
  let vAddSwarmingTypeResult = VAddSwarmingType <$> nameResult <*> sortIndexResult
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
  return (vAddSwarmingTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgAddSwarmingTypeName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    sortIndexFs :: FieldSettings App
    sortIndexFs = FieldSettings
      { fsLabel = SomeMessage MsgAddSwarmingTypeSortIndex
      , fsTooltip = Nothing
      , fsId = Just "sortIndex"
      , fsName = Just "sortIndex"
      , fsAttrs = [ ("class","uk-form-width-medium uk-input uk-form-small") ]
      }

data MsgAddSwarmingType =
  MsgAddSwarmingTypeName
  | MsgAddSwarmingTypeSortIndex

instance RenderMessage App MsgAddSwarmingType where
  renderMessage _ []        = renderAddSwarmingTypeGerman
  renderMessage _ ("de":_) = renderAddSwarmingTypeGerman
  renderMessage _ ("en":_) = renderAddSwarmingTypeEnglish
  renderMessage _ ("en-US":_) = renderAddSwarmingTypeEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderAddSwarmingTypeGerman :: MsgAddSwarmingType -> Text
renderAddSwarmingTypeGerman MsgAddSwarmingTypeName = "Name"
renderAddSwarmingTypeGerman MsgAddSwarmingTypeSortIndex = "Sortierungs-Index"


renderAddSwarmingTypeEnglish :: MsgAddSwarmingType -> Text
renderAddSwarmingTypeEnglish MsgAddSwarmingTypeName = "Name"
renderAddSwarmingTypeEnglish MsgAddSwarmingTypeSortIndex = "Sort Index"

-- gen add form - end

-------------------------------------------------------
-- edit swarming Type
-------------------------------------------------------

-- gen data edit - start
data VEditSwarmingType = VEditSwarmingType
  { vEditSwarmingTypeName :: Text
  , vEditSwarmingTypeSortIndex :: Int
  , vEditSwarmingTypeVersion :: Int
  }
-- gen data edit - end

-- gen get edit form - start
getEditSwarmingTypeFormR :: SwarmingTypeId -> Handler Html
getEditSwarmingTypeFormR swarmingTypeId = do
  swarmingType <- runDB $ get404 swarmingTypeId
  (formWidget, _) <- generateFormPost $ vEditSwarmingTypeForm $ Just swarmingType
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalEditSwarmingType}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ EditSwarmingTypeR swarmingTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get edit form - end

-- gen post edit form - start
postEditSwarmingTypeR :: SwarmingTypeId -> Handler Value
postEditSwarmingTypeR swarmingTypeId = do
  ((result, formWidget), _) <- runFormPost $ vEditSwarmingTypeForm Nothing
  case result of
    FormSuccess vEditSwarmingType -> do
      curTime <- liftIO getCurrentTime
      Entity _ authUser <- requireAuth
      urlRenderer <- getUrlRender
      let persistFields = [
            SwarmingTypeName =. vEditSwarmingTypeName vEditSwarmingType
            , SwarmingTypeSortIndex =. vEditSwarmingTypeSortIndex vEditSwarmingType
            , SwarmingTypeVersion =. vEditSwarmingTypeVersion vEditSwarmingType + 1
            , SwarmingTypeUpdatedAt =. curTime
            , SwarmingTypeUpdatedBy =. userIdent authUser
            ]
      updateCount <- runDB $ updateWhereCount [ SwarmingTypeId ==. swarmingTypeId
                                              , SwarmingTypeVersion ==. vEditSwarmingTypeVersion vEditSwarmingType
                                              ] persistFields
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
        else returnJson $ VFormSubmitStale { fsStaleDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $ VFormSubmitInvalid
        { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml }

-- gen post edit form - end

-- gen edit form - start
vEditSwarmingTypeForm :: Maybe SwarmingType -> Html -> MForm Handler (FormResult VEditSwarmingType, Widget)
vEditSwarmingTypeForm maybeSwarmingType extra = do
  (nameResult, nameView) <- mreq textField
    nameFs
    (swarmingTypeName <$> maybeSwarmingType)
  (sortIndexResult, sortIndexView) <- mreq intField
    sortIndexFs
    (swarmingTypeSortIndex <$> maybeSwarmingType)
  (versionResult, versionView) <- mreq hiddenField
    versionFs
    (swarmingTypeVersion <$> maybeSwarmingType)
  let vEditSwarmingTypeResult = VEditSwarmingType <$> nameResult <*> sortIndexResult <*> versionResult
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
  return (vEditSwarmingTypeResult, formWidget)
  where
    nameFs :: FieldSettings App
    nameFs = FieldSettings
      { fsLabel = SomeMessage MsgEditSwarmingTypeName
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs = [ ("class","uk-form-width-large uk-input uk-form-small") ]
      }
    sortIndexFs :: FieldSettings App
    sortIndexFs = FieldSettings
      { fsLabel = SomeMessage MsgEditSwarmingTypeSortIndex
      , fsTooltip = Nothing
      , fsId = Just "sortIndex"
      , fsName = Just "sortIndex"
      , fsAttrs = [ ("class","uk-form-width-medium uk-input uk-form-small") ]
      }
    versionFs :: FieldSettings App
    versionFs = FieldSettings
      { fsLabel = ""
      , fsTooltip = Nothing
      , fsId = Just "version"
      , fsName = Just "version"
      , fsAttrs = []
      }

data MsgEditSwarmingType =
  MsgEditSwarmingTypeName
  | MsgEditSwarmingTypeSortIndex

instance RenderMessage App MsgEditSwarmingType where
  renderMessage _ []        = renderEditSwarmingTypeGerman
  renderMessage _ ("de":_) = renderEditSwarmingTypeGerman
  renderMessage _ ("en":_) = renderEditSwarmingTypeEnglish
  renderMessage _ ("en-US":_) = renderEditSwarmingTypeEnglish
  renderMessage m (_   :ls) = renderMessage m ls

renderEditSwarmingTypeGerman :: MsgEditSwarmingType -> Text
renderEditSwarmingTypeGerman MsgEditSwarmingTypeName = "Name"
renderEditSwarmingTypeGerman MsgEditSwarmingTypeSortIndex = "Sortierungs-Index"


renderEditSwarmingTypeEnglish :: MsgEditSwarmingType -> Text
renderEditSwarmingTypeEnglish MsgEditSwarmingTypeName = "Name"
renderEditSwarmingTypeEnglish MsgEditSwarmingTypeSortIndex = "Sort Index"

-- gen edit form - end

-------------------------------------------------------
-- delete swarming type
-------------------------------------------------------

-- gen get delete form - start
getDeleteSwarmingTypeFormR :: SwarmingTypeId -> Handler Html
getDeleteSwarmingTypeFormR swarmingTypeId = do
  (formWidget, _) <- generateFormPost $ vDeleteSwarmingTypeForm
  formLayout $ do
    toWidget [whamlet|
      <h1>_{MsgGlobalDeleteSwarmingType}
      <form #modal-form .uk-form-horizontal method=post action=@{AdminR $ DeleteSwarmingTypeR swarmingTypeId}>
        <div #modal-form-widget>
          ^{formWidget}
      |]
-- gen get delete form - end

-- gen post delete form - start
postDeleteSwarmingTypeR :: SwarmingTypeId -> Handler Value
postDeleteSwarmingTypeR swarmingTypeId = do
  runDB $ delete swarmingTypeId
  urlRenderer <- getUrlRender
  returnJson $ VFormSubmitSuccess { fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataJsonR }
-- gen post delete form - end

-- gen delete form - start
vDeleteSwarmingTypeForm :: Html -> MForm Handler (FormResult (), Widget)
vDeleteSwarmingTypeForm extra = do
  let formResult = mempty
  let formWidget = [whamlet|#{extra} _{MsgGlobalReallyDelete}|]
  return (formResult, formWidget)
-- gen delete form - end