-- |
-- 'Options.Applicative' based dispatch code.
--
-- /danger:/ This module is designed to call System.Exit
-- and as such should only be called from the main module.
--
module Smith.Cli.Dispatch (
    dispatch
  ) where

import           Options.Applicative ((<**>))
import qualified Options.Applicative as Options


-- |
-- Runs the provided command line parser with
-- sensible defaults.
--
-- On parser failure, prints error message, usage and
-- exits with a status code of @1@.
--
-- On parser success, the value will be returned.
--
-- FUTURE: Write a sensible multi-path command render.
dispatch :: Options.Parser a -> IO a
dispatch parser = do
  Options.customExecParser
    (Options.prefs . mconcat $ [
        Options.showHelpOnEmpty
      , Options.showHelpOnError
      ])
    (Options.info
      (parser <**> Options.helper)
      (mconcat [
          Options.fullDesc
        , Options.progDesc "Request temporary access using Smith."
        , Options.header "Smith secure access requests."
        ]))
