module Db
  ( initDb
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

insertRsvp :: Connection -> Rsvp -> IO ()
insertRsvp conn r = do
  let dietaryColumn = if dietary r == "" then Nothing else Just (dietary r)
  void $ execute conn
    "INSERT INTO rsvps (name, guest_count, dietary) VALUES (?, ?, ?)"
    (name r, guestCount r, dietaryColumn)
