{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>), some)

import qualified Options.Applicative as Options

import qualified Smith.Cli.Command.Wizard as Wizard
import qualified Smith.Cli.Dispatch as Dispatch
import qualified Smith.Cli.Error as Error
import qualified Smith.Cli.Parser as Parser

import qualified System.Exit as Exit


data Command =
    WizardCommand
    deriving (Eq, Ord, Show)


-- |
-- The 'smith-config' executable, designed for smith users,
-- adminstrators and servers to manage shared configuration.
--
main :: IO ()
main =
  Dispatch.dispatch parser >>= \a ->
    case a of
      WizardCommand -> do
        Error.runOrFlailT Issue.renderIssueError $
          Wizard.wizard
        Exit.exitSuccess


parser :: Options.Parser Command
parser =
  pure WizardCommand
