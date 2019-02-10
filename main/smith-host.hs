{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Options.Applicative as Options

import qualified Smith.Cli.Abort as Abort
import qualified Smith.Cli.Command.Host as Host
import qualified Smith.Cli.Dispatch as Dispatch
import qualified Smith.Cli.Parser as Parser

import qualified Smith.Client as Smith
import           Smith.Client.Data.CertificateAuthority (AuthorityPublicKey (..))
import           Smith.Client.Data.Environment (Environment (..))

import qualified System.Exit as Exit
import qualified System.Posix.Files as Files


data Command =
    FetchAuthorityKeys Environment (Maybe FilePath)
  -- FetchHostKeys
  -- FetchKRL
    deriving (Eq, Ord, Show)


-- |
-- The 'smith-host' executable, designed for hosts that allow
-- Smith to manage access control. This executable can fetch
-- files such as certificate-authorities, host-key certificates
-- and key-revocation-lists for hosts.
--
main :: IO ()
main = do
  Dispatch.dispatch parser >>= \a ->
    case a of
      FetchAuthorityKeys environment output -> do
        smith <- Abort.runOrFlailT Smith.renderSmithConfigureError $
          Smith.configureT
        keys <- Abort.runOrFlailT Host.renderAuthorityError $
          Host.authority smith environment
        case output of
          Nothing ->
            for_ keys $
              Text.putStrLn . getAuthorityPublicKey
          Just file -> do
            Text.writeFile file $ mconcat [
                Text.intercalate "\n" $ getAuthorityPublicKey <$> keys
              , "\n"
              ]
            Files.setFileMode file $
              Files.unionFileModes Files.ownerReadMode Files.ownerWriteMode
        Exit.exitSuccess


parser :: Options.Parser Command
parser =
  FetchAuthorityKeys
    <$> Parser.environment
    <*> Options.optional Parser.caKeysFile
