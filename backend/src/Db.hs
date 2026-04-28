module Db
  ( initDb
  , insertRsvp
  , insertRsvpRequest
  , insertSession
  , lookupSession
  , deleteSession
  , purgeExpiredSessions
  , listInvitees
  , createInvitee
  , updateInvitee
  , deleteInvitee
  , listRsvps
  , linkRsvpInvitee
  , insertVideo
  , listVideos
  , getVideo
  ) where

import           Control.Monad              (void)
import qualified Data.ByteString.Char8      as BC8
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime, addUTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           System.Environment         (lookupEnv)
import           Upload                     (SavedVideo (..))
import           Wedding.Types              (Invitee (..), InviteeInput (..),
                                             LinkInviteeBody (..), Rsvp (..),
                                             RsvpAdmin (..), RsvpRequest (..),
                                             VideoAdmin (..))

instance FromRow Invitee where
  fromRow = Invitee <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow RsvpAdmin where
  fromRow = RsvpAdmin <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow VideoAdmin where
  fromRow = VideoAdmin <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

initDb :: IO Connection
initDb = do
  mUrl <- lookupEnv "DATABASE_URL"
  let connStr = maybe "postgres://localhost/wedding" id mUrl
  connectPostgreSQL (BC8.pack connStr)

insertRsvp :: Connection -> Rsvp -> IO ()
insertRsvp conn r =
  insertRsvpRequest conn RsvpRequest
    { rrName           = name r
    , rrGuestCount     = guestCount r
    , rrDietary        = dietary r
    , rrInvitationCode = Nothing
    }

insertRsvpRequest :: Connection -> RsvpRequest -> IO ()
insertRsvpRequest conn r = do
  mInvitee <- findInvitee conn (rrName r) (rrInvitationCode r)
  let dietaryColumn = nonEmpty (rrDietary r)
      inviteeIdColumn = fmap inviteeId mInvitee
      codeColumn = rrInvitationCode r >>= nonEmpty
  void $ execute conn
    "INSERT INTO rsvps (name, guest_count, dietary, invitee_id, invitation_code_used) VALUES (?, ?, ?, ?, ?)"
    (rrName r, rrGuestCount r, dietaryColumn, inviteeIdColumn, codeColumn)

findInvitee :: Connection -> Text -> Maybe Text -> IO (Maybe Invitee)
findInvitee conn rsvpName mCode = do
  byCode <- case mCode >>= nonEmpty of
    Nothing   -> pure []
    Just code -> query conn inviteeSelectByCode (Only code)
  case byCode of
    (i:_) -> pure (Just i)
    []    -> do
      byName <- query conn inviteeSelectByName (Only rsvpName)
      pure $ case byName of
        []    -> Nothing
        (i:_) -> Just i

insertSession :: Connection -> Text -> IO ()
insertSession conn token = do
  now <- getCurrentTime
  let expires = addUTCTime (24 * 3600) now
  void $ execute conn
    "INSERT INTO admin_sessions (token, expires_at) VALUES (?, ?)"
    (token, expires)

lookupSession :: Connection -> Text -> IO Bool
lookupSession conn token = do
  now <- getCurrentTime
  rows :: [Only Int] <- query conn
    "SELECT 1 FROM admin_sessions WHERE token = ? AND expires_at > ?"
    (token, now)
  pure (not (null rows))

deleteSession :: Connection -> Text -> IO ()
deleteSession conn token =
  void $ execute conn "DELETE FROM admin_sessions WHERE token = ?" (Only token)

purgeExpiredSessions :: Connection -> IO ()
purgeExpiredSessions conn = do
  now <- getCurrentTime
  void $ execute conn "DELETE FROM admin_sessions WHERE expires_at <= ?" (Only now)

listInvitees :: Connection -> IO [Invitee]
listInvitees conn = query_ conn
  "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees ORDER BY name ASC"

createInvitee :: Connection -> InviteeInput -> IO Invitee
createInvitee conn input = oneRow "createInvitee" =<< query conn
  "INSERT INTO invitees (name, code, max_guests, notes) VALUES (?, ?, ?, ?) RETURNING id, name, code, max_guests, notes, created_at::text"
  (iiName input, iiCode input >>= nonEmpty, iiMaxGuests input, iiNotes input >>= nonEmpty)

updateInvitee :: Connection -> Int64 -> InviteeInput -> IO (Maybe Invitee)
updateInvitee conn iid input = do
  rows <- query conn
    "UPDATE invitees SET name = ?, code = ?, max_guests = ?, notes = ? WHERE id = ? RETURNING id, name, code, max_guests, notes, created_at::text"
    (iiName input, iiCode input >>= nonEmpty, iiMaxGuests input, iiNotes input >>= nonEmpty, iid)
  pure $ case rows of
    []    -> Nothing
    (i:_) -> Just i

deleteInvitee :: Connection -> Int64 -> IO ()
deleteInvitee conn iid =
  void $ execute conn "DELETE FROM invitees WHERE id = ?" (Only iid)

listRsvps :: Connection -> IO [RsvpAdmin]
listRsvps conn = query_ conn
  "SELECT id::text, name, guest_count, dietary, invitee_id, invitation_code_used, created_at::text FROM rsvps ORDER BY created_at DESC"

linkRsvpInvitee :: Connection -> Text -> LinkInviteeBody -> IO (Maybe RsvpAdmin)
linkRsvpInvitee conn rid body = do
  rows <- query conn
    "UPDATE rsvps SET invitee_id = ? WHERE id = ?::uuid RETURNING id::text, name, guest_count, dietary, invitee_id, invitation_code_used, created_at::text"
    (linkInviteeId body, rid)
  pure $ case rows of
    []    -> Nothing
    (r:_) -> Just r

insertVideo :: Connection -> SavedVideo -> IO Text
insertVideo conn video = do
  rows :: [Only Text] <- query conn
    "INSERT INTO videos (original_filename, stored_filename, content_type, size_bytes, submitter_name, message) VALUES (?, ?, ?, ?, ?, ?) RETURNING id::text"
    ( savedOriginalFilename video
    , savedStoredFilename video
    , savedContentType video
    , savedSizeBytes video
    , savedSubmitterName video
    , savedMessage video
    )
  case rows of
    []             -> fail "insertVideo: no row returned"
    (Only vid : _) -> pure vid

listVideos :: Connection -> IO [VideoAdmin]
listVideos conn = query_ conn
  "SELECT id::text, original_filename, stored_filename, content_type, size_bytes, submitter_name, message, created_at::text FROM videos ORDER BY created_at DESC"

getVideo :: Connection -> Text -> IO (Maybe VideoAdmin)
getVideo conn vid = do
  rows <- query conn
    "SELECT id::text, original_filename, stored_filename, content_type, size_bytes, submitter_name, message, created_at::text FROM videos WHERE id = ?::uuid"
    (Only vid)
  pure $ case rows of
    []    -> Nothing
    (v:_) -> Just v

inviteeSelectByCode :: Query
inviteeSelectByCode =
  "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees WHERE code = ? LIMIT 1"

inviteeSelectByName :: Query
inviteeSelectByName =
  "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees WHERE LOWER(name) = LOWER(?) ORDER BY created_at ASC LIMIT 1"

oneRow :: String -> [a] -> IO a
oneRow label rows = case rows of
  []    -> fail (label <> ": no row returned")
  (x:_) -> pure x

nonEmpty :: Text -> Maybe Text
nonEmpty value =
  let stripped = T.strip value
   in if T.null stripped then Nothing else Just stripped
