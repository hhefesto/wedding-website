module Db
  ( initDb
  , lookupInvite
  , submitRsvpRequest
  , insertSession
  , lookupSession
  , insertRsvpSession
  , lookupRsvpSession
  , deleteSession
  , purgeExpiredSessions
  , listInvitees
  , getInvitee
  , getInviteeByCode
  , inviteeHasRsvp
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
import           Wedding.Types              (AttendanceStatus (..), InviteLookup (..),
                                             Invitee (..), InviteeInput (..),
                                             LinkInviteeBody (..), RsvpAdmin (..),
                                             RsvpRequest (..), VideoAdmin (..))

instance FromRow Invitee where
  fromRow = Invitee <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow RsvpAdmin where
  fromRow = do
    rid <- field
    rname <- field
    status <- statusFromText <$> field
    count <- field
    rdietary <- field
    rinvitee <- field
    code <- field
    created <- field
    pure (RsvpAdmin rid rname status count rdietary rinvitee code created)

instance FromRow VideoAdmin where
  fromRow = VideoAdmin <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

initDb :: IO Connection
initDb = do
  mUrl <- lookupEnv "DATABASE_URL"
  let connStr = maybe "postgres://localhost/wedding" id mUrl
  connectPostgreSQL (BC8.pack connStr)

lookupInvite :: Connection -> Text -> IO (Maybe InviteLookup)
lookupInvite conn rawCode =
  case nonEmpty rawCode of
    Nothing -> pure Nothing
    Just code -> do
      rows <- query conn
        "SELECT i.name, i.max_guests, r.status, r.guest_count FROM invitees i LEFT JOIN rsvps r ON r.invitee_id = i.id WHERE i.code = ? LIMIT 1"
        (Only code)
      pure $ case rows of
        [] -> Nothing
        ((iname, maxGuests, mStatus, mCount):_) -> Just InviteLookup
          { ilName       = iname
          , ilMaxGuests  = maxGuests
          , ilHasRsvp    = maybe False (const True) (mStatus :: Maybe Text)
          , ilStatus     = statusFromText <$> mStatus
          , ilGuestCount = mCount
          }

submitRsvpRequest :: Connection -> RsvpRequest -> IO (Either Text Invitee)
submitRsvpRequest conn r = do
  case nonEmpty (rrInvitationCode r) of
    Nothing -> pure (Left "El codigo de invitacion es obligatorio.")
    Just code -> do
      mInvitee <- findInviteeByCode conn code
      case mInvitee of
        Nothing -> pure (Left "Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto.")
        Just invitee ->
          case validateRsvp invitee r of
            Left msg -> pure (Left msg)
            Right count -> do
              void $ execute conn
                "INSERT INTO rsvps (name, status, guest_count, dietary, invitee_id, invitation_code_used, updated_at) VALUES (?, ?, ?, ?, ?, ?, NOW()) ON CONFLICT (invitee_id) WHERE invitee_id IS NOT NULL DO UPDATE SET name = EXCLUDED.name, status = EXCLUDED.status, guest_count = EXCLUDED.guest_count, dietary = EXCLUDED.dietary, invitation_code_used = EXCLUDED.invitation_code_used, updated_at = NOW()"
                ( inviteeName invitee
                , statusToText (rrStatus r)
                , count
                , nonEmpty (rrDietary r)
                , inviteeId invitee
                , Just code
                )
              pure (Right invitee)

findInviteeByCode :: Connection -> Text -> IO (Maybe Invitee)
findInviteeByCode conn code = do
  rows <- query conn inviteeSelectByCode (Only code)
  pure $ case rows of
    []    -> Nothing
    (i:_) -> Just i

validateRsvp :: Invitee -> RsvpRequest -> Either Text Int
validateRsvp invitee r =
  case rrStatus r of
    Declined -> Right 0
    Attending
      | rrGuestCount r < 1 -> Left "Confirma al menos un asistente o marca que no podras asistir."
      | rrGuestCount r > inviteeMaxGuests invitee ->
          Left ("Tu invitacion permite hasta " <> T.pack (show (inviteeMaxGuests invitee)) <> " asistentes.")
      | otherwise -> Right (rrGuestCount r)

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

insertRsvpSession :: Connection -> Text -> Int64 -> IO ()
insertRsvpSession conn token iid = do
  now <- getCurrentTime
  let expires = addUTCTime (180 * 24 * 3600) now
  void $ execute conn
    "INSERT INTO rsvp_sessions (token, invitee_id, expires_at) VALUES (?, ?, ?)"
    (token, iid, expires)

lookupRsvpSession :: Connection -> Text -> IO (Maybe Invitee)
lookupRsvpSession conn token = do
  now <- getCurrentTime
  rows <- query conn
    ("SELECT i.id, i.name, i.code, i.max_guests, i.notes, i.created_at::text " <>
     "FROM rsvp_sessions s JOIN invitees i ON i.id = s.invitee_id " <>
     "WHERE s.token = ? AND s.expires_at > ? LIMIT 1")
    (token, now)
  pure $ case rows of
    []    -> Nothing
    (i:_) -> Just i

deleteSession :: Connection -> Text -> IO ()
deleteSession conn token =
  void $ execute conn "DELETE FROM admin_sessions WHERE token = ?" (Only token)

purgeExpiredSessions :: Connection -> IO ()
purgeExpiredSessions conn = do
  now <- getCurrentTime
  void $ execute conn "DELETE FROM admin_sessions WHERE expires_at <= ?" (Only now)
  void $ execute conn "DELETE FROM rsvp_sessions WHERE expires_at <= ?" (Only now)

listInvitees :: Connection -> IO [Invitee]
listInvitees conn = query_ conn
  "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees ORDER BY name ASC"

getInvitee :: Connection -> Int64 -> IO (Maybe Invitee)
getInvitee conn iid = do
  rows <- query conn
    "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees WHERE id = ? LIMIT 1"
    (Only iid)
  pure $ case rows of
    []    -> Nothing
    (i:_) -> Just i

getInviteeByCode :: Connection -> Text -> IO (Maybe Invitee)
getInviteeByCode conn code = do
  rows <- query conn inviteeSelectByCode (Only code)
  pure $ case rows of
    []    -> Nothing
    (i:_) -> Just i

inviteeHasRsvp :: Connection -> Int64 -> IO Bool
inviteeHasRsvp conn iid = do
  rows :: [Only Int] <- query conn
    "SELECT 1 FROM rsvps WHERE invitee_id = ? LIMIT 1"
    (Only iid)
  pure (not (null rows))

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
  "SELECT id::text, name, status, guest_count, dietary, invitee_id, invitation_code_used, created_at::text FROM rsvps ORDER BY created_at DESC"

linkRsvpInvitee :: Connection -> Text -> LinkInviteeBody -> IO (Maybe RsvpAdmin)
linkRsvpInvitee conn rid body = do
  rows <- query conn
    "UPDATE rsvps SET invitee_id = ?, updated_at = NOW() WHERE id = ?::uuid RETURNING id::text, name, status, guest_count, dietary, invitee_id, invitation_code_used, created_at::text"
    (linkInviteeId body, rid)
  pure $ case rows of
    []    -> Nothing
    (r:_) -> Just r

insertVideo :: Connection -> Int64 -> SavedVideo -> IO Text
insertVideo conn iid video = do
  rows :: [Only Text] <- query conn
    "INSERT INTO videos (original_filename, stored_filename, content_type, size_bytes, invitee_id, submitter_name, message) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id::text"
    ( savedOriginalFilename video
    , savedStoredFilename video
    , savedContentType video
    , savedSizeBytes video
    , iid
    , savedSubmitterName video
    , savedMessage video
    )
  case rows of
    []             -> fail "insertVideo: no row returned"
    (Only vid : _) -> pure vid

listVideos :: Connection -> IO [VideoAdmin]
listVideos conn = query_ conn
  "SELECT id::text, original_filename, stored_filename, content_type, size_bytes, invitee_id, submitter_name, message, created_at::text FROM videos ORDER BY created_at DESC"

getVideo :: Connection -> Text -> IO (Maybe VideoAdmin)
getVideo conn vid = do
  rows <- query conn
    "SELECT id::text, original_filename, stored_filename, content_type, size_bytes, invitee_id, submitter_name, message, created_at::text FROM videos WHERE id = ?::uuid"
    (Only vid)
  pure $ case rows of
    []    -> Nothing
    (v:_) -> Just v

inviteeSelectByCode :: Query
inviteeSelectByCode =
  "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees WHERE code = ? LIMIT 1"

oneRow :: String -> [a] -> IO a
oneRow label rows = case rows of
  []    -> fail (label <> ": no row returned")
  (x:_) -> pure x

nonEmpty :: Text -> Maybe Text
nonEmpty value =
  let stripped = T.strip value
   in if T.null stripped then Nothing else Just stripped

statusToText :: AttendanceStatus -> Text
statusToText Attending = "attending"
statusToText Declined  = "declined"

statusFromText :: Text -> AttendanceStatus
statusFromText "declined" = Declined
statusFromText _          = Attending
