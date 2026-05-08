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
  , linkVideoInvitee
  , resolveDuplicateRsvp
  , listVideos
  , getVideo
  , deleteVideo
  , listIpAssociations
  , createIpAssociation
  , updateIpAssociation
  , deleteIpAssociation
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Maybe                 (listToMaybe)
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
                                               IpAssociationAdmin (..), IpAssociationInput (..),
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
    ip <- field
    resolution <- field
    suggestedInviteeId <- field
    suggestedName <- field
    suggestedCode <- field
    suggestedRsvpId <- field
    created <- field
    pure (RsvpAdmin rid rname status count rdietary rinvitee code inviteeName inviteeCode ip resolution suggestedInviteeId suggestedName suggestedCode suggestedRsvpId created)

instance FromRow VideoAdmin where
  fromRow = VideoAdmin <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow IpAssociationAdmin where
  fromRow = IpAssociationAdmin <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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

submitRsvpRequest :: Connection -> RsvpRequest -> Maybe Text -> IO (Either Text SubmittedRsvp)
submitRsvpRequest conn r mIp = withTransaction conn $ do
  let submittedName = nonEmpty (rrName r)
  case nonEmpty (rrInvitationCode r) of
    Nothing ->
      case validateLooseRsvp r of
        Left msg -> pure (Left msg)
        Right count -> submitLooseRsvp conn r mIp submittedName count
    Just code -> do
      mInvitee <- findInviteeByCode conn code
      case mInvitee of
        Nothing ->
          case validateLooseRsvp r of
            Left msg -> pure (Left msg)
            Right count -> submitLooseRsvp conn r { rrInvitationCode = "" } mIp submittedName count
        Just invitee ->
          case validateRsvp invitee r of
            Left msg -> pure (Left msg)
            Right count -> do
              let name = maybe (inviteeName invitee) id submittedName
              rows :: [Only Text] <- query conn
                "INSERT INTO rsvps (name, status, guest_count, dietary, invitee_id, invitation_code_used, ip_address, invitee_resolution_status, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?::inet, 'resolved', NOW()) ON CONFLICT (invitee_id) WHERE invitee_id IS NOT NULL DO UPDATE SET name = EXCLUDED.name, status = EXCLUDED.status, guest_count = EXCLUDED.guest_count, dietary = EXCLUDED.dietary, invitation_code_used = EXCLUDED.invitation_code_used, ip_address = COALESCE(EXCLUDED.ip_address, rsvps.ip_address), invitee_resolution_status = 'resolved', suggested_invitee_id = NULL, suggested_existing_rsvp_id = NULL, updated_at = NOW() RETURNING id::text"
                ( name
                , statusToText (rrStatus r)
                , count
                , nonEmpty (rrDietary r)
                , inviteeId invitee
                , Just code
                , mIp
                )
              let rid = oneOnly "submitRsvpRequest" rows
              maybe (pure ()) (upsertInviteeIp conn (inviteeId invitee) "rsvp_code") mIp
              void $ execute conn "UPDATE videos SET rsvp_id = ?::uuid WHERE invitee_id = ? AND rsvp_id IS NULL" (rid, inviteeId invitee)
              pure (Right (SubmittedRsvp (Just invitee) rid name))

submitLooseRsvp :: Connection -> RsvpRequest -> Maybe Text -> Maybe Text -> Int -> IO (Either Text SubmittedRsvp)
submitLooseRsvp conn r mIp submittedName count = do
  let name = maybe "" id submittedName
      status = statusToText (rrStatus r)
      dietary = nonEmpty (rrDietary r)
  matches <- maybe (pure []) (resolveInviteesByIp conn) mIp
  case listToMaybe matches of
    Nothing -> insertUnlinked (Nothing :: Maybe Int64) (Nothing :: Maybe Text) "unlinked" name status dietary
    Just invitee -> do
      existing <- getRsvpByInvitee conn (inviteeId invitee)
      case existing of
        Just (existingId, existingName, existingStatus, existingCount, existingDietary)
          | sameRsvp name status count dietary existingName existingStatus existingCount existingDietary -> do
              maybe (pure ()) (upsertInviteeIp conn (inviteeId invitee) "rsvp_ip_duplicate") mIp
              pure (Right (SubmittedRsvp (Just invitee) existingId existingName))
          | otherwise -> insertUnlinked (Just (inviteeId invitee)) (Just existingId) "review" name status dietary
        Nothing -> do
          let review = length matches > 1
              resolution :: Text
              resolution = if review then "review" else "resolved"
          rows :: [Only Text] <- query conn
            "INSERT INTO rsvps (name, status, guest_count, dietary, invitee_id, invitation_code_used, ip_address, invitee_resolution_status, suggested_invitee_id, updated_at) VALUES (?, ?, ?, ?, ?, NULL, ?::inet, ?, ?, NOW()) RETURNING id::text"
            (name, status, count, dietary, Just (inviteeId invitee), mIp, resolution, if review then Just (inviteeId invitee) else Nothing)
          maybe (pure ()) (upsertInviteeIp conn (inviteeId invitee) "rsvp_ip") mIp
          pure (Right (SubmittedRsvp (Just invitee) (oneOnly "submitLooseRsvp" rows) name))
  where
    insertUnlinked :: Maybe Int64 -> Maybe Text -> Text -> Text -> Text -> Maybe Text -> IO (Either Text SubmittedRsvp)
    insertUnlinked suggestedInvitee suggestedRsvp resolution name status dietary = do
      rows :: [Only Text] <- query conn
        "INSERT INTO rsvps (name, status, guest_count, dietary, invitee_id, invitation_code_used, ip_address, invitee_resolution_status, suggested_invitee_id, suggested_existing_rsvp_id, updated_at) VALUES (?, ?, ?, ?, NULL, NULL, ?::inet, ?, ?, ?::uuid, NOW()) RETURNING id::text"
        (name, status, count, dietary, mIp, resolution, suggestedInvitee, suggestedRsvp)
      pure (Right (SubmittedRsvp Nothing (oneOnly "submitLooseRsvp" rows) name))

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
   "i.name, i.code, r.ip_address::text, r.invitee_resolution_status, r.suggested_invitee_id, si.name, si.code, r.suggested_existing_rsvp_id::text, r.created_at::text " <>
   "FROM rsvps r LEFT JOIN invitees i ON i.id = r.invitee_id LEFT JOIN invitees si ON si.id = r.suggested_invitee_id ORDER BY COALESCE(r.suggested_existing_rsvp_id, r.id)::text, r.created_at DESC")

linkRsvpInvitee :: Connection -> Text -> LinkInviteeBody -> IO (Maybe RsvpAdmin)
linkRsvpInvitee conn rid body = do
  withTransaction conn $ do
    rows <- query conn
      ("WITH updated AS (UPDATE rsvps SET invitee_id = ?, invitee_resolution_status = ?, suggested_invitee_id = NULL, suggested_existing_rsvp_id = NULL, updated_at = NOW() WHERE id = ?::uuid RETURNING *) " <>
        "SELECT r.id::text, r.name, r.status, r.guest_count, r.dietary, r.invitee_id, r.invitation_code_used, i.name, i.code, r.ip_address::text, r.invitee_resolution_status, r.suggested_invitee_id, si.name, si.code, r.suggested_existing_rsvp_id::text, r.created_at::text " <>
        "FROM updated r LEFT JOIN invitees i ON i.id = r.invitee_id LEFT JOIN invitees si ON si.id = r.suggested_invitee_id")
      (linkInviteeId body, maybe "unlinked" (const "resolved") (linkInviteeId body) :: Text, rid)
    case rows of
      [] -> pure Nothing
      (r:_) -> do
        case linkInviteeId body of
          Nothing ->
            void $ execute conn "UPDATE videos SET invitee_id = NULL, invitee_resolution_status = 'unlinked' WHERE rsvp_id = ?::uuid" (Only rid)
          Just iid -> do
            upsertInviteeIpForRsvp conn iid rid "admin_rsvp"
            void $ execute conn "UPDATE videos SET invitee_id = ?, invitee_resolution_status = 'resolved' WHERE rsvp_id = ?::uuid" (iid, rid)
            void $ execute conn "UPDATE videos SET rsvp_id = ?::uuid WHERE invitee_id = ? AND rsvp_id IS NULL" (rid, iid)
        pure (Just r)

deleteRsvp :: Connection -> Text -> IO ()
deleteRsvp conn rid =
  void $ execute conn "DELETE FROM rsvps WHERE id = ?::uuid" (Only rid)

insertVideo :: Connection -> Maybe RsvpSession -> Maybe Text -> SavedVideo -> IO Text
insertVideo conn mSession mIp video = withTransaction conn $ do
  (miid, resolution) <- resolveVideoInvitee conn mSession mIp (savedInvitationCode video)
  let mrid = mSession >>= sessionRsvpId
      submitter = savedSubmitterName video <|> (mSession >>= sessionInviteeName) <|> (mSession >>= sessionRsvpName)
  rows :: [Only Text] <- query conn
    "INSERT INTO videos (original_filename, stored_filename, content_type, size_bytes, invitee_id, rsvp_id, ip_address, invitee_resolution_status, submitter_name, message) VALUES (?, ?, ?, ?, ?, ?::uuid, ?::inet, ?, ?, ?) RETURNING id::text"
    ( savedOriginalFilename video
    , savedStoredFilename video
    , savedContentType video
    , savedSizeBytes video
    , miid
    , mrid
    , mIp
    , resolution
    , submitter
    , savedMessage video
    )
  case miid of
    Just iid -> maybe (pure ()) (upsertInviteeIp conn iid "video") mIp
    Nothing  -> pure ()
  case rows of
    []             -> fail "insertVideo: no row returned"
    (Only vid : _) -> pure vid

resolveVideoInvitee :: Connection -> Maybe RsvpSession -> Maybe Text -> Maybe Text -> IO (Maybe Int64, Text)
resolveVideoInvitee conn _mSession mIp mCode =
  case mCode >>= nonEmpty of
    Just code -> do
      mInvitee <- findInviteeByCode conn code
      case mInvitee of
        Just invitee -> pure (Just (inviteeId invitee), "resolved")
        Nothing      -> resolveByIp
    Nothing -> resolveByIp
  where
    resolveByIp = do
      matches <- maybe (pure []) (resolveInviteesByIp conn) mIp
      case matches of
        []       -> pure (Nothing, "unlinked")
        [invitee] -> pure (Just (inviteeId invitee), "resolved")
        (invitee:_) -> pure (Just (inviteeId invitee), "review")

linkVideoInvitee :: Connection -> Text -> LinkInviteeBody -> IO (Maybe VideoAdmin)
linkVideoInvitee conn vid body = withTransaction conn $ do
  rows <- query conn
    ("WITH updated AS (UPDATE videos SET invitee_id = ?, invitee_resolution_status = ? WHERE id = ?::uuid RETURNING *) " <>
     "SELECT v.id::text, v.original_filename, v.stored_filename, v.content_type, v.size_bytes, v.invitee_id, v.rsvp_id::text, " <>
     "i.name, i.code, r.name, v.submitter_name, v.message, v.ip_address::text, v.invitee_resolution_status, v.created_at::text " <>
     "FROM updated v LEFT JOIN invitees i ON i.id = v.invitee_id LEFT JOIN rsvps r ON r.id = v.rsvp_id")
    (linkInviteeId body, maybe "unlinked" (const "resolved") (linkInviteeId body) :: Text, vid)
  case (rows, linkInviteeId body) of
    (v:_, Just iid) -> upsertInviteeIpForVideo conn iid vid "admin_video" >> pure (Just v)
    (v:_, Nothing)  -> pure (Just v)
    ([], _)         -> pure Nothing

resolveDuplicateRsvp :: Connection -> Text -> Text -> IO Bool
resolveDuplicateRsvp conn rid keep = withTransaction conn $
  if keep == "new"
    then do
      rows :: [(Maybe Int64, Maybe Text)] <- query conn "SELECT suggested_invitee_id, suggested_existing_rsvp_id::text FROM rsvps WHERE id = ?::uuid LIMIT 1" (Only rid)
      case rows of
        [(Just iid, Just existingRid)] -> do
          void $ execute conn "UPDATE rsvps SET invitee_id = NULL, invitee_resolution_status = 'unlinked', suggested_invitee_id = NULL, suggested_existing_rsvp_id = NULL WHERE id = ?::uuid" (Only existingRid)
          void $ execute conn "UPDATE rsvps SET invitee_id = ?, invitee_resolution_status = 'resolved', suggested_invitee_id = NULL, suggested_existing_rsvp_id = NULL, updated_at = NOW() WHERE id = ?::uuid" (iid, rid)
          upsertInviteeIpForRsvp conn iid rid "admin_duplicate"
          void $ execute conn "UPDATE videos SET rsvp_id = ?::uuid, invitee_id = ?, invitee_resolution_status = 'resolved' WHERE rsvp_id = ?::uuid" (rid, iid, existingRid)
          pure True
        _ -> pure False
    else do
      n <- execute conn "DELETE FROM rsvps WHERE id = ?::uuid AND invitee_resolution_status = 'review'" (Only rid)
      pure (n > 0)

listVideos :: Connection -> IO [VideoAdmin]
listVideos conn = query_ conn
  ("SELECT v.id::text, v.original_filename, v.stored_filename, v.content_type, v.size_bytes, v.invitee_id, v.rsvp_id::text, " <>
   "i.name, i.code, r.name, v.submitter_name, v.message, v.ip_address::text, v.invitee_resolution_status, v.created_at::text " <>
   "FROM videos v LEFT JOIN invitees i ON i.id = v.invitee_id LEFT JOIN rsvps r ON r.id = v.rsvp_id ORDER BY v.created_at DESC")

getVideo :: Connection -> Text -> IO (Maybe VideoAdmin)
getVideo conn vid = do
  rows <- query conn
    ("SELECT v.id::text, v.original_filename, v.stored_filename, v.content_type, v.size_bytes, v.invitee_id, v.rsvp_id::text, " <>
     "i.name, i.code, r.name, v.submitter_name, v.message, v.ip_address::text, v.invitee_resolution_status, v.created_at::text " <>
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

listIpAssociations :: Connection -> IO [IpAssociationAdmin]
listIpAssociations conn = query_ conn
  ("SELECT a.id, a.invitee_id, i.name, i.code, a.ip_address::text, a.source, a.first_seen_at::text, a.last_seen_at::text " <>
   "FROM invitee_ip_addresses a JOIN invitees i ON i.id = a.invitee_id " <>
   "ORDER BY a.last_seen_at DESC, a.id DESC")

createIpAssociation :: Connection -> IpAssociationInput -> IO IpAssociationAdmin
createIpAssociation conn input = oneRow "createIpAssociation" =<< query conn
  ("WITH inserted AS (" <>
   "INSERT INTO invitee_ip_addresses (invitee_id, ip_address, source) VALUES (?, ?::inet, ?) " <>
   "ON CONFLICT (invitee_id, ip_address) DO UPDATE SET last_seen_at = NOW(), source = EXCLUDED.source RETURNING *) " <>
   "SELECT a.id, a.invitee_id, i.name, i.code, a.ip_address::text, a.source, a.first_seen_at::text, a.last_seen_at::text " <>
   "FROM inserted a JOIN invitees i ON i.id = a.invitee_id")
  (ipiInviteeId input, ipiIpAddress input, nonEmptyDefault "admin" (ipiSource input))

updateIpAssociation :: Connection -> Int64 -> IpAssociationInput -> IO (Maybe IpAssociationAdmin)
updateIpAssociation conn aid input = do
  rows <- query conn
    ("WITH updated AS (" <>
     "UPDATE invitee_ip_addresses SET invitee_id = ?, ip_address = ?::inet, source = ?, last_seen_at = NOW() WHERE id = ? RETURNING *) " <>
     "SELECT a.id, a.invitee_id, i.name, i.code, a.ip_address::text, a.source, a.first_seen_at::text, a.last_seen_at::text " <>
     "FROM updated a JOIN invitees i ON i.id = a.invitee_id")
    (ipiInviteeId input, ipiIpAddress input, nonEmptyDefault "admin" (ipiSource input), aid)
  pure $ case rows of
    []    -> Nothing
    (a:_) -> Just a

deleteIpAssociation :: Connection -> Int64 -> IO Bool
deleteIpAssociation conn aid = do
  n <- execute conn "DELETE FROM invitee_ip_addresses WHERE id = ?" (Only aid)
  pure (n > 0)

resolveInviteesByIp :: Connection -> Text -> IO [Invitee]
resolveInviteesByIp conn ip = query conn
  ("SELECT i.id, i.name, i.code, i.max_guests, i.notes, i.created_at::text " <>
   "FROM invitee_ip_addresses a JOIN invitees i ON i.id = a.invitee_id " <>
   "WHERE a.ip_address = ?::inet ORDER BY a.first_seen_at ASC, a.invitee_id ASC")
  (Only ip)

upsertInviteeIp :: Connection -> Int64 -> Text -> Text -> IO ()
upsertInviteeIp conn iid source ip =
  void $ execute conn
    "INSERT INTO invitee_ip_addresses (invitee_id, ip_address, source) VALUES (?, ?::inet, ?) ON CONFLICT (invitee_id, ip_address) DO UPDATE SET last_seen_at = NOW(), source = EXCLUDED.source"
    (iid, ip, source)

upsertInviteeIpForRsvp :: Connection -> Int64 -> Text -> Text -> IO ()
upsertInviteeIpForRsvp conn iid rid source = do
  rows :: [Only Text] <- query conn "SELECT ip_address::text FROM rsvps WHERE id = ?::uuid AND ip_address IS NOT NULL" (Only rid)
  case rows of
    (Only ip:_) -> upsertInviteeIp conn iid source ip
    []          -> pure ()

upsertInviteeIpForVideo :: Connection -> Int64 -> Text -> Text -> IO ()
upsertInviteeIpForVideo conn iid vid source = do
  rows :: [Only Text] <- query conn "SELECT ip_address::text FROM videos WHERE id = ?::uuid AND ip_address IS NOT NULL" (Only vid)
  case rows of
    (Only ip:_) -> upsertInviteeIp conn iid source ip
    []          -> pure ()

getRsvpByInvitee :: Connection -> Int64 -> IO (Maybe (Text, Text, Text, Int, Maybe Text))
getRsvpByInvitee conn iid = do
  rows <- query conn "SELECT id::text, name, status, guest_count, dietary FROM rsvps WHERE invitee_id = ? LIMIT 1" (Only iid)
  pure $ case rows of
    []    -> Nothing
    (r:_) -> Just r

sameRsvp :: Text -> Text -> Int -> Maybe Text -> Text -> Text -> Int -> Maybe Text -> Bool
sameRsvp name status count dietary existingName existingStatus existingCount existingDietary =
  T.strip name == T.strip existingName
    && status == existingStatus
    && count == existingCount
    && fmap T.strip dietary == fmap T.strip existingDietary

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

nonEmptyDefault :: Text -> Text -> Text
nonEmptyDefault fallback value = maybe fallback id (nonEmpty value)

capGuests :: Int -> Int
capGuests = max 1 . min 2

statusToText :: AttendanceStatus -> Text
statusToText Attending = "attending"
statusToText Declined  = "declined"

statusFromText :: Text -> AttendanceStatus
statusFromText "declined" = Declined
statusFromText _          = Attending
