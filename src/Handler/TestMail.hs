{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.TestMail where

import Handler.Common
import Handler.Mailer
import Import
import qualified Text.Blaze.Html.Renderer.Text as Blaze

data Testmail = Testmail
  { testmailEmail :: Text
  }

postSendTestmailR :: Handler Value
postSendTestmailR = do
  ((result, formWidget), _) <- runFormPost $ vSendTestmailForm Nothing
  case result of
    FormSuccess vSendTestmail -> do
      urlRenderer <- getUrlRender
      sendTestMail $ vSendTestmailEmail vSendTestmail
      returnJson $ VFormSubmitSuccess {fsSuccessDataJsonUrl = urlRenderer $ AdminR AdminPageDataR}
    _ -> do
      resultHtml <- formLayout [whamlet|^{formWidget}|]
      returnJson $
        VFormSubmitInvalid
          { fsInvalidModalWidgetHtml = toStrict $ Blaze.renderHtml resultHtml
          }

-- gen data action - start
data VSendTestmail = VSendTestmail
  { vSendTestmailEmail :: Text
  }

-- gen data action - end

-- gen get action form - start
getSendTestmailFormR :: Handler Html
getSendTestmailFormR = do
  (formWidget, _) <- generateFormPost $ vSendTestmailForm Nothing
  formLayout $
    toWidget
      [whamlet|
      <h1>_{MsgTestmailSendTestMail}
      <form #modal-form .uk-form-horizontal method=post onsubmit="return false;" action=@{AdminR $ SendTestmailR }>
        <div #modal-form-widget>
          ^{formWidget}
      |]

-- gen get action form - end

-- gen action form - start
vSendTestmailForm :: Maybe Testmail -> Html -> MForm Handler (FormResult VSendTestmail, Widget)
vSendTestmailForm maybeTestmail extra = do
  (emailResult, emailView) <-
    mreq
      textField
      emailFs
      (testmailEmail <$> maybeTestmail)
  let vSendTestmailResult = VSendTestmail <$> emailResult
  let formWidget =
        toWidget
          [whamlet|
    #{extra}
    <div #emailInputWidget .uk-margin-small :not $ null $ fvErrors emailView:.uk-form-danger>
      <label #emailInputLabel .uk-form-label :not $ null $ fvErrors emailView:.uk-text-danger for=#{fvId emailView}>#{fvLabel emailView}
      <div .uk-form-controls>
        ^{fvInput emailView}
        <span #emailInputInfo .uk-margin-left .uk-text-small .input-info>
          _{MsgTestmailEmailInputInfo}
        $maybe err <- fvErrors emailView
          <br>
          <span #emailInputError .uk-text-small .input-error>
            &nbsp;#{err}
    |]
  return (vSendTestmailResult, formWidget)
  where
    emailFs :: FieldSettings App
    emailFs =
      FieldSettings
        { fsLabel = SomeMessage MsgTestmailEmail,
          fsTooltip = Nothing,
          fsId = Just "email",
          fsName = Just "email",
          fsAttrs = [("class", "uk-input uk-form-small uk-form-width-large")]
        }
-- gen action form - end
