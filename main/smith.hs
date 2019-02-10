{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>), some)

import qualified Options.Applicative as Options

import qualified Smith.Cli.Command.Issue as Issue
import qualified Smith.Cli.Configuration as Configuration
import           Smith.Cli.Data.Program (Program (..))
import qualified Smith.Cli.Dispatch as Dispatch
import qualified Smith.Cli.Error as Error
import qualified Smith.Cli.Parser as Parser

import qualified Smith.Client as Smith
import           Smith.Client.Data.CertificateRequest (Principal (..))
import           Smith.Client.Data.Environment (Environment (..))

import qualified System.Exit as Exit


data Command =
    Command Environment [Principal] (Maybe Program)
    deriving (Eq, Ord, Show)


-- |
-- The 'smith' executable, designed for smith users who
-- want to request access to servers.
--
main :: IO ()
main = do
  principal <- Configuration.configureDefaultPrincipal
  Dispatch.dispatch (parser principal) >>= \a ->
    case a of
      Command environment principals program -> do
        smith <- Error.runOrFlailT Smith.renderSmithConfigureError $
          Smith.configureT
        Error.runOrFlailT Issue.renderIssueError $
          Issue.issue smith environment principals program
        Exit.exitSuccess


parser :: Principal -> Options.Parser Command
parser principal =
  let
    issue =
      Command
        <$> Parser.environment
        <*> ((some Parser.principal) <|> pure [principal])
        <*> pure Nothing

    exec =
      Command
        <$> Parser.environment
        <*> ((some Parser.principal) <|> pure [principal])
        <*> fmap Just Parser.program
  in
    issue <|> exec
