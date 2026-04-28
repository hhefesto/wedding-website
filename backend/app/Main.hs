module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)

import           Api                      (app)
import           Db                       (initDb)

main :: IO ()
main = do
  conn <- initDb
  port <- maybe 3001 read <$> lookupEnv "WEDDING_PORT"
  putStrLn $ "wedding-backend listening on port " <> show port
  Warp.run port (app conn)
