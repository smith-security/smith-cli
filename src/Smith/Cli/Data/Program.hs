-- |
-- Data types respresenting a program to execute.
--
module Smith.Cli.Data.Program (
    Program (..)
  ) where

import           Data.Text (Text)

data Program =
    Program Text [Text]
    deriving (Eq, Ord, Show)
