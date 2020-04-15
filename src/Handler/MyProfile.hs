{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.MyProfile where

import Database.Persist.Sql (updateWhereCount)
import Handler.Common
import Handler.Mailer
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

postEditMyprofileR :: Handler Value
postEditMyprofileR = do
  ((result, formWidget), _) <- runFormPost $ vEditMyprofileForm Nothing
  case result of
    FormSuccess vEditMyprofile -> do
      curTime <- liftIO getCurrentTime
      Entity userId authUser <- requireAuth
      (passwd, passwdHash) <- liftIO $ generatePassword 32
      urlRenderer <- getUrlRender
      let persistFields =
            [ UserEmail =. vEditMyprofileEmail vEditMyprofile,
              UserVersion =. vEditMyprofileVersion vEditMyprofile + 1,
              UserUpdatedAt =. curTime,
              UserUpdatedBy =. userIdent authUser
            ]
      let persistFields' =
            persistFields
              ++ [ UserPassword =. Just passwdHash
                   | vEditMyprofileIsResetPassword vEditMyprofile
                 ]
      updateCount <-
        runDB $
          updateWhereCount
            [ UserId ==. userId,
              UserVersion ==. vEditMyprofileVersion vEditMyprofile
            ]
            persistFields'
      when (vEditMyprofileIsResetPassword vEditMyprofile) $ do
        user' <- runDB $ get404 userId
        sendPasswordResetMail user' passwd
      if updateCount == 1
        then returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ HiverecR HomePageDataJsonR}
        else returnJson $ VFormSubmitStale {fsStaleDataJsonUrl = urlRenderer $ HiverecR HomePageDataJsonR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

data VEditMyprofile = VEditMyprofile
  { vEditMyprofileEmail :: Text,
    vEditMyprofileIsResetPassword :: Bool,
    vEditMyprofileVersion :: Int
  }
  deriving (Show)

getEditMyprofileFormR :: Handler Html
getEditMyprofileFormR = do
  Entity userId _ <- requireAuth
  myProfile <- runDB $ get404 userId
  (formWidget, _) <- generateFormPost $ vEditMyprofileForm $ Just myProfile
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgGlobalEditMyProfile}
      <form #modal-form .uk-form-horizontal method=post action=@{HiverecR $ EditMyprofileR}>
        <div #modal-form-widget>
          ^{formWidget}
      |]

vEditMyprofileForm :: Maybe User -> Html -> MForm Handler (FormResult VEditMyprofile, Widget)
vEditMyprofileForm maybeUser extra = do
  (emailResult, emailView) <-
    mreq
      textField
      vMyprofileEmailFieldSettings
      (userEmail <$> maybeUser)
  (isResetPasswordResult, isResetPasswordView) <-
    mreq
      checkBoxField
      vMyprofileIsResetPasswordFieldSettings
      Nothing
  (versionResult, versionView) <-
    mreq
      hiddenField
      vMyprofileVersionFieldSettings
      (userVersion <$> maybeUser)
  let vEditMyprofileResult = VEditMyprofile <$> emailResult <*> isResetPasswordResult <*> versionResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    ^{fvInput versionView}
    <div .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=email>Email
      <div .uk-form-controls>
        ^{fvInput emailView}
        $maybe err <- fvErrors emailView
          &nbsp;#{err}
    <div .uk-margin-small :not $ null $ fvErrors isResetPasswordView:.uk-form-danger>
      <label .uk-form-label :not $ null $ fvErrors isResetPasswordView:.uk-text-danger for=isResetPassword>Neues Passwort generieren?
      <div .uk-form-controls>
        ^{fvInput isResetPasswordView}
        $maybe err <- fvErrors isResetPasswordView
          &nbsp;#{err}
    |]
  return (vEditMyprofileResult, formWidget)
  where
    vMyprofileEmailFieldSettings :: FieldSettings App
    vMyprofileEmailFieldSettings =
      FieldSettings
        { fsLabel = "Email",
          fsTooltip = Nothing,
          fsId = Just "email",
          fsName = Just "email",
          fsAttrs = [("class", "uk-form-width-large uk-input uk-form-small")]
        }
    vMyprofileIsResetPasswordFieldSettings :: FieldSettings App
    vMyprofileIsResetPasswordFieldSettings =
      FieldSettings
        { fsLabel = "Neues Passwort generieren?",
          fsTooltip = Nothing,
          fsId = Just "isResetPassword",
          fsName = Just "isResetPassword",
          fsAttrs = [("class", "uk-checkbox")]
        }
    vMyprofileVersionFieldSettings :: FieldSettings App
    vMyprofileVersionFieldSettings =
      FieldSettings
        { fsLabel = "",
          fsTooltip = Nothing,
          fsId = Just "version",
          fsName = Just "version",
          fsAttrs = []
        }
