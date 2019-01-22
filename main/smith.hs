{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>), some)

import qualified Options.Applicative as Options

import qualified Smith.Cli.Command.Issue as Issue
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
main =
  Dispatch.dispatch parser >>= \a ->
    case a of
      Command environment principals program -> do
        smith <- Error.runOrFlailT Smith.renderSmithConfigureError $
          Smith.configureT
        Error.runOrFlailT Issue.renderIssueError $
          Issue.issue smith environment principals program
        Exit.exitSuccess


-- FIX default to current user, not root?
parser :: Options.Parser Command
parser =
  Command
    <$> Parser.environment
    <*> ((some Parser.principal) <|> pure [Principal "root"])
    <*> Options.optional Parser.program
