-- |
-- Command line configuration.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Configuration (
    Configuration (..)
  , configure
  , configureDefaultPrincipal
  ) where

import qualified Control.Monad.Catch as Catch

import qualified Data.Text as Text

import           Smith.Client.Data.CertificateRequest (Principal (..))

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath (FilePath, (</>))
import qualified System.Posix.User as User


data Configuration =
  Configuration {
      configurationSmithHome :: FilePath
    } deriving (Eq, Ord, Show)


configure :: IO Configuration
configure = do
  Environment.lookupEnv "SMITH_HOME" >>=
    maybe
      (Configuration <$> defaultSmithHome)
      (pure . Configuration)

configureDefaultPrincipal :: IO Principal
configureDefaultPrincipal =
  let
    fallback =
      maybe "root" id <$>
        Environment.lookupEnv "USER"
    current =
      Catch.handleIOError (\_ -> fallback) $
        (User.getLoginName)
    override =
      Environment.lookupEnv "SMITH_PRINCIPAL"
  in
    fmap (Principal . Text.pack) $
      override >>= maybe current pure

defaultSmithHome :: IO FilePath
defaultSmithHome =
  Directory.getHomeDirectory >>= \home ->
    pure (home </> ".smith")
