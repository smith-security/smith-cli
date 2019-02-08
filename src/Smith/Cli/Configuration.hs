-- |
-- Command line configuration.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Configuration (
    Configuration (..)
  , configure
  ) where

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath (FilePath, (</>))


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


defaultSmithHome :: IO FilePath
defaultSmithHome =
  Directory.getHomeDirectory >>= \home ->
    pure (home </> ".smith")
