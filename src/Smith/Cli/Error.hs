-- |
-- Error handling routines.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Error (
    renderSmithError
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Smith.Client.Error (SmithError (..), ErrorCode (..))


-- |
-- Render a smith client error in a way suitable for
-- command line output.
--
renderSmithError :: SmithError -> Text
renderSmithError e =
  case e of
    -- FUTURE: debug mode that prints message.
    -- FUTURE: Handle specific error codes for better error messages.
    SmithApplicationError code _message ->
      mconcat ["There was an error performing your request [", getErrorCode code, "]."]
    -- FUTURE: debug mode that prints message.
    SmithAuthorizationError code _message ->
      mconcat ["You are not authorized to perform this request [", getErrorCode code, "]."]
    SmithAuthenticationError _err ->
      mconcat ["Smith could not authenticate you, please check your credentials and connectivity to Smith. DEBUG: ", Text.pack . show $ _err]
    -- FUTURE: debug mode that prints body + message
    SmithResponseParseError code _body _message ->
      mconcat ["Smith response parse error [", Text.pack . show $ code, "]. Please check connectivity to Smith and retry request."]
    -- FUTURE: debug mode that prints body.
    SmithStatusCodeError code _body ->
      mconcat ["Smith status code error [", Text.pack . show $ code, "]. Please check connectivity to Smith and retry request."]
    SmithUrlParseError message ->
      mconcat ["Smith client url-parse error [", message, "]. Check you are running the latest client version, and raise a supportissue if this issue persists ."]
