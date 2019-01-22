-- |
-- Agent operations.
--
-- FUTURE: This should move to openssh-protocol.
--
{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.Agent (
    Agent
  , AddKeyError (..)
  , Message (..)
  , messageByte
  , Reply (..)
  , replyByte
  , connect
  , addKey
  , packet
  , send
  ) where

import           Control.Monad (void, unless)
import           Control.Monad.Trans.Bifunctor (BifunctorTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Control.Monad.IO.Class (MonadIO (..))

import qualified Crypto.OpenSSH.Protocol.Decode as Decode
import qualified Crypto.OpenSSH.Protocol.Encode as Encode

import qualified Data.Attoparsec.Text as Parser
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

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

data Message =
    AddIdentityMessage
    deriving (Eq, Ord, Show)

data Reply =
    FailureReply
  | SuccessReply
  | ExtensionFailureReply
  | IdentitiesAnswerReply
  | SignResponseReply
    deriving (Eq, Ord, Show)

messageByte :: Message -> Word8
messageByte m =
  case m of
    AddIdentityMessage ->
      17

replyByte :: Reply -> Word8
replyByte r =
  case r of
    FailureReply ->
      5
    SuccessReply ->
      6
    ExtensionFailureReply ->
      28
    IdentitiesAnswerReply ->
      12
    SignResponseReply ->
      14

packet :: Message -> Serialize.Put -> Serialize.Put
packet message payload =
  Encode.string . Serialize.runPut $ do
    Serialize.putWord8 $ messageByte message
    payload

data ProtocolError =
    InvalidResponseHeader ByteString
    deriving (Eq, Ord, Show)

send :: Agent -> Message -> Serialize.Put -> ExceptT ProtocolError IO ByteString
send (Agent agent) message payload = do
  liftIO . Socket.sendAll agent . Serialize.runPut $
    packet message payload
  header <- liftIO $ Socket.recv agent 4
  size <- ExceptT . pure . first (const $ InvalidResponseHeader header) $
    Serialize.runGet (Decode.uint32) header
  liftIO $ Socket.recv agent (fromIntegral size)

data AddKeyError =
    IncompleteKeyPair
  | InvalidCertificate
  | CouldNotAddKey
  | CouldNotAddCertificate
  | AddKeyProtocolError ProtocolError
    deriving (Eq, Ord, Show)

addKey :: Agent -> Certificate -> OpenSSL.RSAKeyPair -> ExceptT AddKeyError IO ()
addKey agent cert keys = do
  (DeconstructedCertificate t blob comment) <- maybe (left InvalidCertificate) pure $ deconstruct cert
  let
    n = OpenSSL.rsaN keys
    e = OpenSSL.rsaE keys
    d = OpenSSL.rsaD keys
    p = OpenSSL.rsaP keys
    q = OpenSSL.rsaQ keys
  iqmp <- maybe (left IncompleteKeyPair) pure $ OpenSSL.rsaIQMP keys

  rkey <- firstT AddKeyProtocolError $
    send agent AddIdentityMessage $ do
      Encode.string "ssh-rsa"
      Encode.mpint n
      Encode.mpint e
      Encode.mpint d
      Encode.mpint iqmp
      Encode.mpint p
      Encode.mpint q
      -- FUTURE Better default comment, server should send one.
      Encode.text $ maybe "" id comment

  unless (rkey == ByteString.singleton (replyByte SuccessReply)) $
    left CouldNotAddKey

  rcertificate <- firstT AddKeyProtocolError $
    send agent AddIdentityMessage $ do
      Encode.text t
      Encode.string blob
      Encode.mpint d
      Encode.mpint iqmp
      Encode.mpint p
      Encode.mpint q
      -- FUTURE Better default comment, server should send one.
      Encode.text $ maybe "" id comment

  unless (rcertificate == ByteString.singleton (replyByte SuccessReply)) $
    left CouldNotAddCertificate

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
