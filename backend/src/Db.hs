module Db
  ( initDb
  , createSchema
  , insertRsvp
  ) where

import           Control.Monad              (void)
import qualified Data.ByteString.Char8      as BC8
import           Database.PostgreSQL.Simple
import           System.Environment         (lookupEnv)
import           Wedding.Types              (Rsvp (..))

initDb :: IO Connection
initDb = do
  mUrl <- lookupEnv "DATABASE_URL"
  let connStr = maybe "postgres://localhost/wedding" id mUrl
  connectPostgreSQL (BC8.pack connStr)

createSchema :: Connection -> IO ()
createSchema conn = do
  void $ execute_ conn "CREATE EXTENSION IF NOT EXISTS pgcrypto"
  void $ execute_ conn
    "CREATE TABLE IF NOT EXISTS rsvps \
    \( id          UUID        PRIMARY KEY DEFAULT gen_random_uuid() \
    \, name        TEXT        NOT NULL                              \
    \, guest_count INT         NOT NULL CHECK (guest_count BETWEEN 1 AND 20) \
    \, dietary     TEXT                                              \
    \, created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()                \
    \, client_ip   INET                                              \
    \, user_agent  TEXT                                              \
    \)"

insertRsvp :: Connection -> Rsvp -> IO ()
insertRsvp conn r = do
  let dietaryColumn = if dietary r == "" then Nothing else Just (dietary r)
  void $ execute conn
    "INSERT INTO rsvps (name, guest_count, dietary) VALUES (?, ?, ?)"
    (name r, guestCount r, dietaryColumn)
