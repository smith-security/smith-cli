-- |
-- Command for requesting a certificate.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Command.Wizard (
    -- * Entry point
    wizard

    -- * Errors
  , WizardError (..)
  , renderWizardError
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))

import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Smith.Cli.Configuration as Configuration

import qualified System.IO as IO


-- |
-- Interactive wizard for creating smith configuration file.
--
wizard :: ExceptT WizardError IO ()
wizard = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  liftIO $ Text.putStr . mconcat $  [
      "Default environment (current: ", "none", "): "
    ]
  environment <- Environment <$> liftIO Text.getLine

  liftIO $ Text.putStr . mconcat $  [
      "Default principals (current: ", "none", "): "
    ]
  environment <- Environment <$> liftIO Text.getLine




data WizardError =
    WizardError
    deriving (Eq, Show)

renderWizardError :: WizardError -> Text
renderWizardError e =
  case e of
    WizardError ->
      "todo"

left :: Applicative m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
