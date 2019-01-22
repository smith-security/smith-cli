{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Agent (
    Agent
  , AddKeyError (..)
  , connect
  , addKey
  ) where


import           Control.Monad (void)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.IO.Class (MonadIO (..))

import qualified Crypto.OpenSSH.Protocol.Encode as Encode

import qualified Data.Attoparsec.Text as Parser
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Char as Char
import qualified Data.Serialize as Serialize
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Traversable (for)
import           Data.Word (Word8)

import           Network.Socket (Socket)
import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString as Socket

import qualified OpenSSL.RSA as OpenSSL

import           Smith.Client.Data.Certificate (Certificate (..))

import qualified System.Environment as Environment
import           System.IO (IO)


data DeconstructedCertificate =
  DeconstructedCertificate {
      _certificateType :: Text
    , _certificateBlob :: ByteString
    , _certificateComment :: (Maybe Text)
    } deriving (Eq, Ord, Show)

data Agent =
  Agent Socket


connect :: IO (Maybe Agent)
connect = do
  path <- Environment.lookupEnv "SSH_AUTH_SOCK"
  for path $ \address -> do
    socket <- Socket.socket Socket.AF_UNIX Socket.Stream 0
    Socket.connect socket $ Socket.SockAddrUnix address
    pure $ Agent socket

data AgentMessage =
  SshAgentcAddIdentity

messageByte :: AgentMessage -> Word8
messageByte m =
  case m of
    SshAgentcAddIdentity ->
      17

data AddKeyError =
    IncompleteKeyPair
  | InvalidCertificate
    deriving (Eq, Ord, Show)

addKey :: Agent -> Certificate -> OpenSSL.RSAKeyPair -> ExceptT AddKeyError IO ()
addKey (Agent agent) cert keys = do
  -- FIX check type
  (DeconstructedCertificate _t blob comment) <- maybe (left InvalidCertificate) pure $ deconstruct cert
  let
    n = OpenSSL.rsaN keys
    e = OpenSSL.rsaE keys
    d = OpenSSL.rsaD keys
    p = OpenSSL.rsaP keys
    q = OpenSSL.rsaQ keys
  iqmp <- maybe (left IncompleteKeyPair) pure $ OpenSSL.rsaIQMP keys

  liftIO $ Socket.sendAll agent $
    Serialize.runPut $ Encode.string $ Serialize.runPut $ do
      Serialize.putWord8 $ messageByte SshAgentcAddIdentity
      Encode.string "ssh-rsa"
      Encode.mpint n
      Encode.mpint e
      Encode.mpint d
      Encode.mpint iqmp
      Encode.mpint p
      Encode.mpint q
      -- FIX better default comment
      Encode.text $ maybe "" id comment

  -- FIX check message length
  _words <- liftIO $ Socket.recv agent 4
  --bytes <- liftIO $ Serialize.runGet (Decode.uint32) words
  _word <- liftIO $ Socket.recv agent 1 {-- bytes --}
  -- FIX check response code.

  liftIO $ Socket.sendAll agent $
    Serialize.runPut $ Encode.string $ Serialize.runPut $ do
      Serialize.putWord8 $ messageByte SshAgentcAddIdentity
      Encode.string "ssh-rsa-cert-v01@openssh.com"
      Encode.string blob
      Encode.mpint d
      Encode.mpint iqmp
      Encode.mpint p
      Encode.mpint q
      -- FIX better default comment
      Encode.text $ maybe "" id comment

  -- FIX check message length
  __words <- liftIO $ Socket.recv agent 4
  --bytes <- liftIO $ Serialize.runGet (Decode.uint32) words
  __word <- liftIO $ Socket.recv agent 1 {-- bytes --}
  -- FIX check response code.
  pure ()

deconstruct :: Certificate -> Maybe DeconstructedCertificate
deconstruct certificate =
  either (const Nothing) Just . flip Parser.parseOnly (getCertificate certificate) $ do
    t <- Parser.takeTill (Char.isSpace)
    void $ Parser.takeWhile (Char.isSpace)
    base64 <- Parser.takeTill (Char.isSpace)
    blob <- case Base64.decode . Text.encodeUtf8 $ base64 of
      Left e ->
        fail e
      Right v ->
        pure v
    void $ Parser.takeWhile (Char.isSpace)
    comment <- Parser.takeTill (Char.isSpace)
    pure $ DeconstructedCertificate t blob
      (if Text.null comment then Nothing else Just comment)

left :: Monad m => x -> ExceptT x m a
left =
  ExceptT . pure . Left
