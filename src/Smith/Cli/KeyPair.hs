{-# LANGUAGE OverloadedStrings #-}
module Smith.Cli.KeyPair (
    EncodedRSAKeyPair (..)
  , Comment (..)
  , newRSAKeyPair
  , encodeRSAKeyPair
  ) where

import qualified Crypto.PubKey.OpenSsh as OpenSsh
import qualified Crypto.Types.PubKey.RSA as RSA

import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified OpenSSL.RSA as OpenSSL

import           System.IO (IO)


newtype Comment =
  Comment {
      getComment :: Text
    } deriving (Eq, Ord, Show)

data EncodedRSAKeyPair =
  EncodedRSAKeyPair {
      sshRSAPublicKey :: Text
    , sshRSAPrivateKey :: Text
    } deriving (Eq, Ord, Show)

-- RSA exponent matches ssh-keygen, see OpenSSL constant RSA_F4.
-- https://github.com/openssh/openssh-portable/blob/7d68e262944c1fff1574600fe0e5e92ec8b398f5/sshkey.c#L1518
newRSAKeyPair :: IO OpenSSL.RSAKeyPair
newRSAKeyPair =
  OpenSSL.generateRSAKey'
    4096
    (0x10001 {- OpenSSL.RSA_F4 = 65537 = 0x10001 -})

encodeRSAKeyPair :: Comment -> OpenSSL.RSAKeyPair -> Maybe EncodedRSAKeyPair
encodeRSAKeyPair comment openssl =
  let
    public =
      RSA.PublicKey {
          RSA.public_size = OpenSSL.rsaSize openssl
        , RSA.public_n = OpenSSL.rsaN openssl
        , RSA.public_e = OpenSSL.rsaE openssl
        }
    mprivate = do
      dP <- OpenSSL.rsaDMP1 openssl
      dQ <- OpenSSL.rsaDMQ1 openssl
      qinv <- OpenSSL.rsaIQMP openssl
      pure $ RSA.PrivateKey {
          RSA.private_pub = public
        , RSA.private_d = OpenSSL.rsaD openssl
        , RSA.private_p = OpenSSL.rsaP openssl
        , RSA.private_q = OpenSSL.rsaQ openssl
        , RSA.private_dP = dP
        , RSA.private_dQ = dQ
        , RSA.private_qinv = qinv
        }
  in
    flip fmap mprivate $ \private ->
      EncodedRSAKeyPair
        (Text.decodeUtf8 . OpenSsh.encodePublic . OpenSsh.OpenSshPublicKeyRsa public . Text.encodeUtf8 . getComment $ comment)
        (Text.decodeUtf8 . OpenSsh.encodePrivate $ OpenSsh.OpenSshPrivateKeyRsa private)
