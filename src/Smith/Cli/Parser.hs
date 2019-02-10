-- |
-- 'Options.Applicative' based parsers for use within
-- the command line tools. This are centralised to
-- help with consistency across various commands.
--
-- This module is designed to be import qualified.
--
-- > import qualified Smith.Cli.Parser as Parser
--
module Smith.Cli.Parser (
  -- * flags
    environment
  , principal

  -- * arguments
  , program
  , caKeysFile
  ) where

import           Control.Applicative (many)

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Options.Applicative as Options

import           Smith.Cli.Data.Program (Program (..))
import           Smith.Client.Data.Environment (Environment (..))
import           Smith.Client.Data.CertificateRequest (Principal (..))

import           System.IO (FilePath)


environment :: Options.Parser Environment
environment =
 fmap Environment . fmap Text.pack . Options.strOption . mconcat $ [
      Options.long "environment"
    , Options.short 'e'
    , Options.metavar "ENVIRONMENT"
    ]

-- CONSIDER: 'user' alias or similar.
principal :: Options.Parser Principal
principal =
 fmap Principal . fmap Text.pack . Options.strOption . mconcat $ [
      Options.long "principal"
    , Options.short 'p'
    , Options.metavar "PRINCIPAL"
    ]

program :: Options.Parser Program
program =
  Program
    <$> programName
    <*> many programArgument

programName :: Options.Parser Text
programName =
  fmap Text.pack . Options.strArgument . mconcat $ [
      Options.metavar "PROGRAM"
    , Options.help "Program to execute."
    ]

programArgument :: Options.Parser Text
programArgument =
 fmap Text.pack . Options.strArgument . mconcat $ [
      Options.metavar "ARGUMENT"
    , Options.help "Argument to provided program."
    ]

caKeysFile :: Options.Parser FilePath
caKeysFile =
 Options.strArgument . mconcat $ [
      Options.metavar "FILE"
    , Options.help "Output path for certificate authority public keys file."
    ]
