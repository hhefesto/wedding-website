module Main where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.MVar  (newMVar, withMVar)
import           Control.Exception        (SomeException, try)
import           Control.Monad            (forever)
import           Data.String              (fromString)
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)
import           System.IO                (hPutStrLn, stderr)

import           Api                      (AppConfig (..), app)
import           Auth                     (getPasswordHash)
import qualified Db
import           Media                    (MediaConfig (..), ensureMediaDirs, newMediaRuntime,
                                           removePartFile, startMediaWorkers)

main :: IO ()
main = do
  conn <- Db.initDb
  port <- maybe 3001 read <$> lookupEnv "WEDDING_PORT"
  pwHash <- getPasswordHash
  videoDir <- maybe "/var/lib/wedding/videos" id <$> lookupEnv "WEDDING_VIDEO_DIR"
  videoMaxBytes <- maybe (95 * 1024 * 1024) read <$> lookupEnv "WEDDING_VIDEO_MAX_BYTES"
  cookieSecure <- maybe False parseBool <$> lookupEnv "WEDDING_COOKIE_SECURE"
  qrencodeBin <- maybe "qrencode" id <$> lookupEnv "WEDDING_QRENCODE_BIN"
  publicBaseUrl <- maybe "http://wedding.local" id <$> lookupEnv "WEDDING_PUBLIC_BASE_URL"
  mediaCfg <- MediaConfig
    <$> (maybe "/var/lib/wedding/media" id <$> lookupEnv "WEDDING_MEDIA_DIR")
    <*> (maybe (4 * 1024 * 1024 * 1024) read <$> lookupEnv "WEDDING_MEDIA_MAX_BYTES")
    <*> (maybe (5 * 1024 * 1024 * 1024) read <$> lookupEnv "WEDDING_MEDIA_MIN_FREE_BYTES")
  connVar <- newMVar conn
  ensureMediaDirs mediaCfg
  mediaRuntime <- newMediaRuntime
  startMediaWorkers mediaCfg mediaRuntime connVar
  _ <- forkIO $ forever $ do
    threadDelay (3600 * 1000000)
    result <- try $ do
      withMVar connVar Db.purgeExpiredSessions
      stale <- withMVar connVar Db.purgeStaleMediaUploads
      mapM_ (removePartFile mediaCfg) stale
    either (\e -> hPutStrLn stderr ("purge failed: " <> show (e :: SomeException))) pure result
  let cfg = AppConfig
        { appAdminPasswordHash = pwHash
        , appVideoDir          = videoDir
        , appVideoMaxBytes     = videoMaxBytes
        , appCookieSecure      = cookieSecure
        , appQrencodeBin       = qrencodeBin
        , appPublicBaseUrl     = fromString publicBaseUrl
        , appMedia             = mediaCfg
        , appMediaRuntime      = mediaRuntime
        }
      -- Slow phone uploads: allow 2 minutes of silence before dropping.
      settings = Warp.setPort port . Warp.setTimeout 120 $ Warp.defaultSettings
  putStrLn $ "wedding-backend listening on port " <> show port
  Warp.runSettings settings (app cfg connVar)

parseBool :: String -> Bool
parseBool value = value `elem` ["1", "true", "TRUE", "yes", "YES"]
