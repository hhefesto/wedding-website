{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( generateToken
  , getPasswordHash
  , verifyPassword
  ) where

import           Crypto.BCrypt            (validatePassword)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BC8
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.UUID                (toText)
import           Data.UUID.V4             (nextRandom)
import           System.Environment       (lookupEnv)
import           System.IO                (hPutStrLn, stderr)

getPasswordHash :: IO ByteString
getPasswordHash = do
  mFile <- lookupEnv "WEDDING_ADMIN_PASSWORD_HASH_FILE"
  mEnv  <- lookupEnv "WEDDING_ADMIN_PASSWORD_HASH"
  case (mFile, mEnv) of
    (Just path, _) -> BC8.pack <$> readFile path
    (_, Just hash) | not (null hash) -> pure (BC8.pack hash)
    _ -> do
      hPutStrLn stderr "WEDDING_ADMIN_PASSWORD_HASH is not set; using development admin password."
      pure defaultPasswordHash

defaultPasswordHash :: ByteString
defaultPasswordHash =
  "$2y$14$k0vAsyrPC1omeJj8UZeK1uuDHHPbF5GwRcsLXoOHH/7pdkhaRyce6"

verifyPassword :: ByteString -> Text -> Bool
verifyPassword hash password =
  validatePassword (BC8.takeWhile (/= '\n') hash) (BC8.pack (T.unpack password))

generateToken :: IO Text
generateToken = toText <$> nextRandom
