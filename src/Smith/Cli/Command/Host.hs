-- |
-- Command for requesting smith managed host files.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Command.Host (
    -- * Entry point
    authority

    -- * Errors
  , AuthorityError (..)
  , renderAuthorityError
  ) where

import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))

import           Data.Text (Text)

import qualified Smith.Cli.Error as Error

import           Smith.Client (Smith (..))
import qualified Smith.Client as Smith
import           Smith.Client.Error (SmithError (..))
import           Smith.Client.Data.CertificateAuthority (AuthorityPublicKey (..))
import           Smith.Client.Data.Environment (Environment (..))


-- |
-- Requests all authority public keys for specified environment.
--
authority :: Smith -> Environment -> ExceptT AuthorityError IO [AuthorityPublicKey]
authority smith environment = do
  firstT SmithAuthorityError .
    Smith.runRequestT smith . Smith.keys $
      environment

data AuthorityError =
    SmithAuthorityError SmithError
    deriving (Eq, Show)

renderAuthorityError :: AuthorityError -> Text
renderAuthorityError e =
  case e of
    SmithAuthorityError err ->
      Error.renderSmithError err
