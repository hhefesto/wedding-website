module Main where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.MVar  (newMVar)
import           Control.Monad            (forever)
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)

import           Api                      (AppConfig (..), app)
import           Auth                     (getPasswordHash)
import qualified Db

main :: IO ()
main = do
  conn <- Db.initDb
  port <- maybe 3001 read <$> lookupEnv "WEDDING_PORT"
  pwHash <- getPasswordHash
  videoDir <- maybe "/var/lib/wedding/videos" id <$> lookupEnv "WEDDING_VIDEO_DIR"
  videoMaxBytes <- maybe (200 * 1024 * 1024) read <$> lookupEnv "WEDDING_VIDEO_MAX_BYTES"
  cookieSecure <- maybe False parseBool <$> lookupEnv "WEDDING_COOKIE_SECURE"
  connVar <- newMVar conn
  _ <- forkIO $ forever $ do
    threadDelay (3600 * 1000000)
    Db.purgeExpiredSessions conn
  let cfg = AppConfig
        { appAdminPasswordHash = pwHash
        , appVideoDir          = videoDir
        , appVideoMaxBytes     = videoMaxBytes
        , appCookieSecure      = cookieSecure
        }
  putStrLn $ "wedding-backend listening on port " <> show port
  Warp.run port (app cfg connVar)

parseBool :: String -> Bool
parseBool value = value `elem` ["1", "true", "TRUE", "yes", "YES"]
