-- |
-- Command line configuration.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Configuration (
    Configuration (..)
  , SmithHome (..)
  , configure
  , home
  ) where

import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map

import           Smith.Client.Data.CertificateRequest (Principal (..))
import           Smith.Client.Data.Environment (Environment (..))

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath (FilePath, (</>))


data GlobalDefault =
  GlobalDefault {
      defaultEnvironment :: Maybe Environment
    , defaultStartingEnvironment :: EnvironmentDefault
    } deriving (Eq, Ord, Show)

data EnvironmentDefault =
  EnvironmentDefault {
      defaultPrincipals :: Maybe [Principal]
    } deriving (Eq, Ord, Show)

data Configuration =
  Configuration {
      configurationSmithHome :: SmithHome
    , configurationGlobalDefault :: GlobalDefault
    , configurationEnvironmentDefault :: Map Environment EnvironmentDefault
    } deriving (Eq, Ord, Show)

newtype SmithHome =
  SmithHome {
      smithHome :: FilePath
    } deriving (Eq, Ord, Show)

renderSmithHome :: SmithHome -> Text
renderSmithHome =
  Text.pack . smithHome

renderSmithHomeCompact :: SmithHome -> IO Text
renderSmithHomeCompact home = do
  tilde <- Directory.getHomeDirectory
  pure $ Text.replace (Text.pack tilde) "~" (renderSmithHome home)

configure :: IO Configuration
configure =
  Configuration
    <$> configureSmithHome
    <*> pure (GlobalDefault Nothing (EnvironmentDefault []))
    <*> pure (Map.fromList [])

configureSmithHome :: IO SmithHome
configureSmithHome =
  Environment.lookupEnv "SMITH_HOME" >>=
    maybe
      defaultSmithHome
      (pure . SmithHome)

defaultSmithHome :: IO SmithHome
defaultSmithHome =
  Directory.getHomeDirectory >>= \home ->
    pure (SmithHome $ home </> ".smith")
