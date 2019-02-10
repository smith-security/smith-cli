-- |
-- Command line error handling routines.
--
-- /danger:/ This module is designed to call System.Exit
-- and as such should only be called from the main module.
--
module Smith.Cli.Abort (
    flail
  , runOrFlailT
  , runOrFlail
  ) where

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import           Data.Text (Text)
import qualified Data.Text.IO as Text

import qualified System.Exit as Exit
import qualified System.IO as IO


-- |
-- On left case, the handler will be used to print error
-- message to 'System.IO.stderr' and exit with a status
-- code of @1@.
--
-- On right case, the value will be returned.
--
runOrFlailT :: (e -> Text) -> ExceptT e IO a -> IO a
runOrFlailT handler =
  runOrFlail handler . runExceptT


-- |
-- On left case, the handler will be used to print error
-- message to 'System.IO.stderr' and exit with a status
-- code of @1@.
--
-- On right case, the value will be returned.
--
runOrFlail :: (e -> Text) -> IO (Either e a) -> IO a
runOrFlail handler action =
  action >>= either (flail . handler) pure


-- |
-- Print error message to 'System.IO.stderr' and exit with
-- a status code of @1@.
--
flail :: Text -> IO a
flail msg = do
  Text.hPutStrLn IO.stderr msg
  Exit.exitFailure
