-- |
-- Command for requesting a certificate.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Command.Issue (
    -- * Entry point
    issue

    -- * Errors
  , IssueError (..)
  , renderIssueError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))

import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Smith.Cli.Agent as Agent
import           Smith.Cli.Data.Program (Program (..))
import qualified Smith.Cli.Error as Error
import           Smith.Cli.KeyPair (Comment (..), EncodedRSAKeyPair (..))
import qualified Smith.Cli.KeyPair as KeyPair

import           Smith.Client (Smith (..))
import qualified Smith.Client as Smith
import           Smith.Client.Error (SmithError (..))
import           Smith.Client.Data.CertificateRequest (Principal (..), PublicKey (..), CertificateRequest (..))
import           Smith.Client.Data.Environment (Environment (..))

import qualified System.Posix.Process as Posix


-- |
-- Requests a certificate, registering it with ssh-agent if possible.
--
-- If an ssh-agent is not currently running, it is started with
-- output to 'System.IO.stdout' suitable for use with eval.
--
-- If a program is provided, the program is executed with an
-- ssh-agent configured.
--
issue :: Smith -> Environment -> [Principal] -> Maybe Program -> ExceptT IssueError IO ()
issue smith environment principals program = do
  agent <- liftIO Agent.connect >>= maybe (left AgentNotAvailableError) pure
  keys <- liftIO KeyPair.newRSAKeyPair
  encoded <- maybe (left KeyGenerationError) pure $
    KeyPair.encodeRSAKeyPair (Comment . mconcat $ ["smith-", getEnvironment environment]) keys
  certificate <- firstT SmithIssueError .
    Smith.runRequestT smith . Smith.issue $
      CertificateRequest
        (PublicKey $ sshRSAPublicKey encoded)
        principals
        environment
        Nothing
  firstT IssueAddKeyError $
    Agent.addKey agent certificate keys
  liftIO $ for_ program exec

exec :: Program -> IO a
exec (Program command arguments) =
  Posix.executeFile
    (Text.unpack command)
    True
    (Text.unpack <$> arguments)
    Nothing

data IssueError =
    SmithIssueError SmithError
  | AgentNotAvailableError
  | KeyGenerationError
  | IssueAddKeyError Agent.AddKeyError
    deriving (Eq, Show)

renderIssueError :: IssueError -> Text
renderIssueError e =
  case e of
    SmithIssueError err ->
      Error.renderSmithError err
    AgentNotAvailableError ->
      "ssh-agent was not available, ensure it is running and SSH_AUTH_SOCK is set."
    KeyGenerationError ->
      "We couldn't generate a key-pair using OpenSSL for this access request, this is very unusual, try again and please raise a support issue if this persists."
    IssueAddKeyError Agent.IncompleteKeyPair ->
      "An invalid key-pair has been generated (via OpenSSL), this very unusual, try again and please raise a support issue if this persists."
    IssueAddKeyError Agent.InvalidCertificate ->
      "Smith has generated an invalid certificate, please ensure you are running a compatible client version. If running the correct client version please raise a support issue."
    IssueAddKeyError Agent.CouldNotAddKey ->
      "Smith has generated a key but could not add it to your ssh-agent. Please raise a support issue, and include your ssh-agent implementation and version."
    IssueAddKeyError Agent.CouldNotAddCertificate ->
      "Smith has generated a certificate but could not add it to your ssh-agent. Please raise a support issue, and include your ssh-agent implementation and version. Note that if you are using the gnome ssh-agent instead of openssh, it does not support certificates."
    IssueAddKeyError (Agent.AddKeyProtocolError _) ->
      "Smith had a problem talking to your ssh-agent and could not add key or certificate. Please raise a support issue, and include your ssh-agent implementation and version."


left :: Applicative m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
