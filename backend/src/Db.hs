module Db
  ( initDb
  , SubmittedRsvp (..)
  , RsvpSession (..)
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
  , deleteRsvp
  , insertVideo
  , listVideos
  , getVideo
  , deleteVideo
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

data SubmittedRsvp = SubmittedRsvp
  { submittedInvitee :: Maybe Invitee
  , submittedRsvpId  :: Text
  , submittedName    :: Text
  }

data RsvpSession = RsvpSession
  { sessionInviteeId   :: Maybe Int64
  , sessionInviteeName :: Maybe Text
  , sessionInviteeCode :: Maybe Text
  , sessionRsvpId      :: Maybe Text
  , sessionRsvpName    :: Maybe Text
  }

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
    inviteeName <- field
    inviteeCode <- field
    created <- field
    pure (RsvpAdmin rid rname status count rdietary rinvitee code inviteeName inviteeCode created)

instance FromRow VideoAdmin where
  fromRow = VideoAdmin <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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

submitRsvpRequest :: Connection -> RsvpRequest -> IO (Either Text SubmittedRsvp)
submitRsvpRequest conn r = do
  let submittedName = nonEmpty (rrName r)
  case nonEmpty (rrInvitationCode r) of
    Nothing ->
      case validateLooseRsvp r of
        Left msg -> pure (Left msg)
        Right count -> do
          name <- case submittedName of
            Nothing -> pure ""
            Just n  -> pure n
          rows :: [Only Text] <- query conn
            "INSERT INTO rsvps (name, status, guest_count, dietary, invitee_id, invitation_code_used, updated_at) VALUES (?, ?, ?, ?, NULL, NULL, NOW()) RETURNING id::text"
            (name, statusToText (rrStatus r), count, nonEmpty (rrDietary r))
          pure (Right (SubmittedRsvp Nothing (oneOnly "submitRsvpRequest" rows) name))
    Just code -> do
      mInvitee <- findInviteeByCode conn code
      case mInvitee of
        Nothing -> pure (Left "Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto.")
        Just invitee ->
          case validateRsvp invitee r of
            Left msg -> pure (Left msg)
            Right count -> do
              let name = maybe (inviteeName invitee) id submittedName
              rows :: [Only Text] <- query conn
                "INSERT INTO rsvps (name, status, guest_count, dietary, invitee_id, invitation_code_used, updated_at) VALUES (?, ?, ?, ?, ?, ?, NOW()) ON CONFLICT (invitee_id) WHERE invitee_id IS NOT NULL DO UPDATE SET name = EXCLUDED.name, status = EXCLUDED.status, guest_count = EXCLUDED.guest_count, dietary = EXCLUDED.dietary, invitation_code_used = EXCLUDED.invitation_code_used, updated_at = NOW() RETURNING id::text"
                ( name
                , statusToText (rrStatus r)
                , count
                , nonEmpty (rrDietary r)
                , inviteeId invitee
                , Just code
                )
              let rid = oneOnly "submitRsvpRequest" rows
              void $ execute conn "UPDATE videos SET rsvp_id = ?::uuid WHERE invitee_id = ? AND rsvp_id IS NULL" (rid, inviteeId invitee)
              pure (Right (SubmittedRsvp (Just invitee) rid name))

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
          Left ("Tu invitacion permite hasta " <> T.pack (show (inviteeMaxGuests invitee)) <> " adultos.")
      | otherwise -> Right (rrGuestCount r)

validateLooseRsvp :: RsvpRequest -> Either Text Int
validateLooseRsvp r
  | T.null (T.strip (rrName r)) = Left "Escribe tu nombre para registrar tu RSVP."
  | otherwise = case rrStatus r of
      Declined -> Right 0
      Attending
        | rrGuestCount r < 1 -> Left "Confirma al menos un adulto o marca que no podran asistir."
        | rrGuestCount r > 2 -> Left "Cada RSVP permite hasta 2 adultos."
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

insertRsvpSession :: Connection -> Text -> Maybe Int64 -> Maybe Text -> IO ()
insertRsvpSession conn token miid mrid = do
  now <- getCurrentTime
  let expires = addUTCTime (180 * 24 * 3600) now
  void $ execute conn
    "INSERT INTO rsvp_sessions (token, invitee_id, rsvp_id, expires_at) VALUES (?, ?, ?::uuid, ?)"
    (token, miid, mrid, expires)

lookupRsvpSession :: Connection -> Text -> IO (Maybe RsvpSession)
lookupRsvpSession conn token = do
  now <- getCurrentTime
  rows <- query conn
    ("SELECT i.id, i.name, i.code, s.rsvp_id::text, r.name " <>
     "FROM rsvp_sessions s LEFT JOIN invitees i ON i.id = s.invitee_id LEFT JOIN rsvps r ON r.id = s.rsvp_id " <>
     "WHERE s.token = ? AND s.expires_at > ? LIMIT 1")
    (token, now)
  pure $ case rows of
    [] -> Nothing
    ((miid, iname, icode, mrid, rname):_) -> Just (RsvpSession miid iname icode mrid rname)

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
  (iiName input, iiCode input >>= nonEmpty, capGuests (iiMaxGuests input), iiNotes input >>= nonEmpty)

updateInvitee :: Connection -> Int64 -> InviteeInput -> IO (Maybe Invitee)
updateInvitee conn iid input = do
  rows <- query conn
    "UPDATE invitees SET name = ?, code = ?, max_guests = ?, notes = ? WHERE id = ? RETURNING id, name, code, max_guests, notes, created_at::text"
    (iiName input, iiCode input >>= nonEmpty, capGuests (iiMaxGuests input), iiNotes input >>= nonEmpty, iid)
  pure $ case rows of
    []    -> Nothing
    (i:_) -> Just i

deleteInvitee :: Connection -> Int64 -> IO ()
deleteInvitee conn iid =
  void $ execute conn "DELETE FROM invitees WHERE id = ?" (Only iid)

listRsvps :: Connection -> IO [RsvpAdmin]
listRsvps conn = query_ conn
  ("SELECT r.id::text, r.name, r.status, r.guest_count, r.dietary, r.invitee_id, r.invitation_code_used, " <>
   "i.name, i.code, r.created_at::text FROM rsvps r LEFT JOIN invitees i ON i.id = r.invitee_id ORDER BY r.created_at DESC")

linkRsvpInvitee :: Connection -> Text -> LinkInviteeBody -> IO (Maybe RsvpAdmin)
linkRsvpInvitee conn rid body = do
  withTransaction conn $ do
    rows <- query conn
      ("WITH updated AS (UPDATE rsvps SET invitee_id = ?, updated_at = NOW() WHERE id = ?::uuid RETURNING *) " <>
       "SELECT r.id::text, r.name, r.status, r.guest_count, r.dietary, r.invitee_id, r.invitation_code_used, i.name, i.code, r.created_at::text " <>
       "FROM updated r LEFT JOIN invitees i ON i.id = r.invitee_id")
      (linkInviteeId body, rid)
    case rows of
      [] -> pure Nothing
      (r:_) -> do
        case linkInviteeId body of
          Nothing ->
            void $ execute conn "UPDATE videos SET invitee_id = NULL WHERE rsvp_id = ?::uuid" (Only rid)
          Just iid -> do
            void $ execute conn "UPDATE videos SET invitee_id = ? WHERE rsvp_id = ?::uuid" (iid, rid)
            void $ execute conn "UPDATE videos SET rsvp_id = ?::uuid WHERE invitee_id = ? AND rsvp_id IS NULL" (rid, iid)
        pure (Just r)

deleteRsvp :: Connection -> Text -> IO ()
deleteRsvp conn rid =
  void $ execute conn "DELETE FROM rsvps WHERE id = ?::uuid" (Only rid)

insertVideo :: Connection -> RsvpSession -> SavedVideo -> IO Text
insertVideo conn session video = do
  rows :: [Only Text] <- query conn
    "INSERT INTO videos (original_filename, stored_filename, content_type, size_bytes, invitee_id, rsvp_id, submitter_name, message) VALUES (?, ?, ?, ?, ?, ?::uuid, ?, ?) RETURNING id::text"
    ( savedOriginalFilename video
    , savedStoredFilename video
    , savedContentType video
    , savedSizeBytes video
    , sessionInviteeId session
    , sessionRsvpId session
    , savedSubmitterName video
    , savedMessage video
    )
  case rows of
    []             -> fail "insertVideo: no row returned"
    (Only vid : _) -> pure vid

listVideos :: Connection -> IO [VideoAdmin]
listVideos conn = query_ conn
  ("SELECT v.id::text, v.original_filename, v.stored_filename, v.content_type, v.size_bytes, v.invitee_id, v.rsvp_id::text, " <>
   "i.name, i.code, r.name, v.submitter_name, v.message, v.created_at::text " <>
   "FROM videos v LEFT JOIN invitees i ON i.id = v.invitee_id LEFT JOIN rsvps r ON r.id = v.rsvp_id ORDER BY v.created_at DESC")

getVideo :: Connection -> Text -> IO (Maybe VideoAdmin)
getVideo conn vid = do
  rows <- query conn
    ("SELECT v.id::text, v.original_filename, v.stored_filename, v.content_type, v.size_bytes, v.invitee_id, v.rsvp_id::text, " <>
     "i.name, i.code, r.name, v.submitter_name, v.message, v.created_at::text " <>
     "FROM videos v LEFT JOIN invitees i ON i.id = v.invitee_id LEFT JOIN rsvps r ON r.id = v.rsvp_id WHERE v.id = ?::uuid")
    (Only vid)
  pure $ case rows of
    []    -> Nothing
    (v:_) -> Just v

deleteVideo :: Connection -> Text -> IO (Maybe Text)
deleteVideo conn vid = do
  rows <- query conn "DELETE FROM videos WHERE id = ?::uuid RETURNING stored_filename" (Only vid)
  pure $ case rows of
    []             -> Nothing
    (Only name:_) -> Just name

inviteeSelectByCode :: Query
inviteeSelectByCode =
  "SELECT id, name, code, max_guests, notes, created_at::text FROM invitees WHERE code = ? LIMIT 1"

oneRow :: String -> [a] -> IO a
oneRow label rows = case rows of
  []    -> fail (label <> ": no row returned")
  (x:_) -> pure x

oneOnly :: String -> [Only a] -> a
oneOnly label rows = case rows of
  []          -> error (label <> ": no row returned")
  (Only x:_) -> x

nonEmpty :: Text -> Maybe Text
nonEmpty value =
  let stripped = T.strip value
   in if T.null stripped then Nothing else Just stripped

capGuests :: Int -> Int
capGuests = max 1 . min 2

statusToText :: AttendanceStatus -> Text
statusToText Attending = "attending"
statusToText Declined  = "declined"

statusFromText :: Text -> AttendanceStatus
statusFromText "declined" = Declined
statusFromText _          = Attending
