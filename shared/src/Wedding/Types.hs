module Wedding.Types
  ( Rsvp (..)
  , AttendanceStatus (..)
  , RsvpRequest (..)
  , InviteLookup (..)
  , Invitee (..)
  , InviteeInput (..)
  , LoginRequest (..)
  , RsvpLoginRequest (..)
  , RsvpAdmin (..)
  , VideoAdmin (..)
  , IpAssociationAdmin (..)
  , IpAssociationInput (..)
  , LinkInviteeBody (..)
  , ResolveDuplicateBody (..)
  , VideoSubmittedResponse (..)
  , MediaKind (..)
  , MediaUploadInit (..)
  , MediaUploadStarted (..)
  , MediaUploadProgress (..)
  , MediaItem (..)
  , MediaAdmin (..)
  , MediaHiddenBody (..)
  ) where

import           Data.Aeson   (FromJSON (..), ToJSON (..), object, withObject,
                                withText, (.:), (.:?), (.!=), (.=))
import           Data.Int     (Int64)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Rsvp = Rsvp
  { name       :: Text
  , guestCount :: Int
  , dietary    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Rsvp
instance FromJSON Rsvp

data AttendanceStatus = Attending | Declined
  deriving (Eq, Show, Generic)

instance ToJSON AttendanceStatus where
  toJSON Attending = "attending"
  toJSON Declined  = "declined"

instance FromJSON AttendanceStatus where
  parseJSON = withText "AttendanceStatus" $ \value ->
    case value of
      "attending" -> pure Attending
      "declined"  -> pure Declined
      _           -> fail "AttendanceStatus must be attending or declined"

data RsvpRequest = RsvpRequest
  { rrName           :: Text
  , rrInvitationCode :: Text
  , rrStatus         :: AttendanceStatus
  , rrGuestCount     :: Int
  , rrDietary        :: Text
  , rrGuestNames     :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON RsvpRequest where
  toJSON r = object
    [ "name"           .= rrName r
    , "invitationCode" .= rrInvitationCode r
    , "status"         .= rrStatus r
    , "guestCount"     .= rrGuestCount r
    , "dietary"        .= rrDietary r
    , "guestNames"     .= rrGuestNames r
    ]

instance FromJSON RsvpRequest where
  parseJSON = withObject "RsvpRequest" $ \o ->
    RsvpRequest
      <$> o .:? "name" .!= ""
      <*> o .:? "invitationCode" .!= ""
      <*> o .:  "status"
      <*> o .:  "guestCount"
      <*> o .:? "dietary" .!= ""
      <*> o .:? "guestNames" .!= []

data InviteLookup = InviteLookup
  { ilName       :: Text
  , ilMaxGuests  :: Int
  , ilHasRsvp    :: Bool
  , ilStatus     :: Maybe AttendanceStatus
  , ilGuestCount :: Maybe Int
  } deriving (Eq, Show, Generic)

instance ToJSON InviteLookup where
  toJSON i = object
    [ "name"       .= ilName i
    , "maxGuests"  .= ilMaxGuests i
    , "hasRsvp"    .= ilHasRsvp i
    , "status"     .= ilStatus i
    , "guestCount" .= ilGuestCount i
    ]

instance FromJSON InviteLookup where
  parseJSON = withObject "InviteLookup" $ \o ->
    InviteLookup
      <$> o .:  "name"
      <*> o .:  "maxGuests"
      <*> o .:  "hasRsvp"
      <*> o .:? "status"
      <*> o .:? "guestCount"

data Invitee = Invitee
  { inviteeId        :: Int64
  , inviteeName      :: Text
  , inviteeCode      :: Maybe Text
  , inviteeMaxGuests :: Int
  , inviteeNotes     :: Maybe Text
  , inviteeCreatedAt :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Invitee where
  toJSON i = object
    [ "id"        .= inviteeId i
    , "name"      .= inviteeName i
    , "code"      .= inviteeCode i
    , "maxGuests" .= inviteeMaxGuests i
    , "notes"     .= inviteeNotes i
    , "createdAt" .= inviteeCreatedAt i
    ]

instance FromJSON Invitee where
  parseJSON = withObject "Invitee" $ \o ->
    Invitee
      <$> o .:  "id"
      <*> o .:  "name"
      <*> o .:? "code"
      <*> o .:  "maxGuests"
      <*> o .:? "notes"
      <*> o .:  "createdAt"

data InviteeInput = InviteeInput
  { iiName      :: Text
  , iiCode      :: Maybe Text
  , iiMaxGuests :: Int
  , iiNotes     :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON InviteeInput where
  parseJSON = withObject "InviteeInput" $ \o ->
    InviteeInput
      <$> o .:  "name"
      <*> o .:? "code"
      <*> o .:? "maxGuests" .!= 1
      <*> o .:? "notes"

instance ToJSON InviteeInput where
  toJSON i = object
    [ "name"      .= iiName i
    , "code"      .= iiCode i
    , "maxGuests" .= iiMaxGuests i
    , "notes"     .= iiNotes i
    ]

newtype LoginRequest = LoginRequest
  { loginPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> LoginRequest <$> o .: "password"

instance ToJSON LoginRequest where
  toJSON r = object ["password" .= loginPassword r]

newtype RsvpLoginRequest = RsvpLoginRequest
  { rsvpLoginCode :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RsvpLoginRequest where
  parseJSON = withObject "RsvpLoginRequest" $ \o -> RsvpLoginRequest <$> o .: "code"

instance ToJSON RsvpLoginRequest where
  toJSON r = object ["code" .= rsvpLoginCode r]

data RsvpAdmin = RsvpAdmin
  { raId                 :: Text
  , raName               :: Text
  , raStatus             :: AttendanceStatus
  , raGuestCount         :: Int
  , raDietary            :: Maybe Text
  , raInviteeId          :: Maybe Int64
  , raInvitationCodeUsed :: Maybe Text
  , raInviteeName        :: Maybe Text
  , raInviteeCode        :: Maybe Text
  , raIpAddress          :: Maybe Text
  , raResolutionStatus   :: Text
  , raSuggestedInviteeId :: Maybe Int64
  , raSuggestedName      :: Maybe Text
  , raSuggestedCode      :: Maybe Text
  , raSuggestedRsvpId    :: Maybe Text
  , raCreatedAt          :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON RsvpAdmin where
  toJSON r = object
    [ "id"                 .= raId r
    , "name"               .= raName r
    , "status"             .= raStatus r
    , "guestCount"         .= raGuestCount r
    , "dietary"            .= raDietary r
    , "inviteeId"          .= raInviteeId r
    , "invitationCodeUsed" .= raInvitationCodeUsed r
    , "inviteeName"        .= raInviteeName r
    , "inviteeCode"        .= raInviteeCode r
    , "ipAddress"          .= raIpAddress r
    , "resolutionStatus"   .= raResolutionStatus r
    , "suggestedInviteeId" .= raSuggestedInviteeId r
    , "suggestedName"      .= raSuggestedName r
    , "suggestedCode"      .= raSuggestedCode r
    , "suggestedRsvpId"    .= raSuggestedRsvpId r
    , "createdAt"          .= raCreatedAt r
    ]

instance FromJSON RsvpAdmin where
  parseJSON = withObject "RsvpAdmin" $ \o ->
    RsvpAdmin
      <$> o .:  "id"
      <*> o .:  "name"
      <*> o .:  "status"
      <*> o .:  "guestCount"
      <*> o .:? "dietary"
      <*> o .:? "inviteeId"
      <*> o .:? "invitationCodeUsed"
      <*> o .:? "inviteeName"
      <*> o .:? "inviteeCode"
      <*> o .:? "ipAddress"
      <*> o .:  "resolutionStatus"
      <*> o .:? "suggestedInviteeId"
      <*> o .:? "suggestedName"
      <*> o .:? "suggestedCode"
      <*> o .:? "suggestedRsvpId"
      <*> o .:  "createdAt"

newtype LinkInviteeBody = LinkInviteeBody
  { linkInviteeId :: Maybe Int64
  } deriving (Eq, Show, Generic)

instance FromJSON LinkInviteeBody where
  parseJSON = withObject "LinkInviteeBody" $ \o -> LinkInviteeBody <$> o .:? "inviteeId"

instance ToJSON LinkInviteeBody where
  toJSON body = object ["inviteeId" .= linkInviteeId body]

newtype ResolveDuplicateBody = ResolveDuplicateBody
  { resolveKeep :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ResolveDuplicateBody where
  parseJSON = withObject "ResolveDuplicateBody" $ \o -> ResolveDuplicateBody <$> o .: "keep"

instance ToJSON ResolveDuplicateBody where
  toJSON body = object ["keep" .= resolveKeep body]

data VideoAdmin = VideoAdmin
  { vaId               :: Text
  , vaOriginalFilename :: Text
  , vaStoredFilename   :: Text
  , vaContentType      :: Text
  , vaSizeBytes        :: Int64
  , vaInviteeId        :: Maybe Int64
  , vaRsvpId           :: Maybe Text
  , vaInviteeName      :: Maybe Text
  , vaInviteeCode      :: Maybe Text
  , vaRsvpName         :: Maybe Text
  , vaSubmitterName    :: Maybe Text
  , vaMessage          :: Maybe Text
  , vaIpAddress        :: Maybe Text
  , vaResolutionStatus :: Text
  , vaCreatedAt        :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON VideoAdmin where
  toJSON v = object
    [ "id"               .= vaId v
    , "originalFilename" .= vaOriginalFilename v
    , "storedFilename"   .= vaStoredFilename v
    , "contentType"      .= vaContentType v
    , "sizeBytes"        .= vaSizeBytes v
    , "inviteeId"        .= vaInviteeId v
    , "rsvpId"           .= vaRsvpId v
    , "inviteeName"      .= vaInviteeName v
    , "inviteeCode"      .= vaInviteeCode v
    , "rsvpName"         .= vaRsvpName v
    , "submitterName"    .= vaSubmitterName v
    , "message"          .= vaMessage v
    , "ipAddress"        .= vaIpAddress v
    , "resolutionStatus" .= vaResolutionStatus v
    , "createdAt"        .= vaCreatedAt v
    ]

instance FromJSON VideoAdmin where
  parseJSON = withObject "VideoAdmin" $ \o ->
    VideoAdmin
      <$> o .:  "id"
      <*> o .:  "originalFilename"
      <*> o .:  "storedFilename"
      <*> o .:  "contentType"
      <*> o .:  "sizeBytes"
      <*> o .:? "inviteeId"
      <*> o .:? "rsvpId"
      <*> o .:? "inviteeName"
      <*> o .:? "inviteeCode"
      <*> o .:? "rsvpName"
      <*> o .:? "submitterName"
      <*> o .:? "message"
      <*> o .:? "ipAddress"
      <*> o .:  "resolutionStatus"
      <*> o .:  "createdAt"

data IpAssociationAdmin = IpAssociationAdmin
  { ipaId          :: Int64
  , ipaInviteeId   :: Int64
  , ipaInviteeName :: Text
  , ipaInviteeCode :: Maybe Text
  , ipaIpAddress   :: Text
  , ipaSource      :: Text
  , ipaFirstSeenAt :: Text
  , ipaLastSeenAt  :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON IpAssociationAdmin where
  toJSON a = object
    [ "id"          .= ipaId a
    , "inviteeId"   .= ipaInviteeId a
    , "inviteeName" .= ipaInviteeName a
    , "inviteeCode" .= ipaInviteeCode a
    , "ipAddress"   .= ipaIpAddress a
    , "source"      .= ipaSource a
    , "firstSeenAt" .= ipaFirstSeenAt a
    , "lastSeenAt"  .= ipaLastSeenAt a
    ]

instance FromJSON IpAssociationAdmin where
  parseJSON = withObject "IpAssociationAdmin" $ \o ->
    IpAssociationAdmin
      <$> o .:  "id"
      <*> o .:  "inviteeId"
      <*> o .:  "inviteeName"
      <*> o .:? "inviteeCode"
      <*> o .:  "ipAddress"
      <*> o .:  "source"
      <*> o .:  "firstSeenAt"
      <*> o .:  "lastSeenAt"

data IpAssociationInput = IpAssociationInput
  { ipiInviteeId :: Int64
  , ipiIpAddress :: Text
  , ipiSource    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON IpAssociationInput where
  toJSON i = object
    [ "inviteeId" .= ipiInviteeId i
    , "ipAddress" .= ipiIpAddress i
    , "source"    .= ipiSource i
    ]

instance FromJSON IpAssociationInput where
  parseJSON = withObject "IpAssociationInput" $ \o ->
    IpAssociationInput
      <$> o .:  "inviteeId"
      <*> o .:  "ipAddress"
      <*> o .:? "source" .!= "admin"

newtype VideoSubmittedResponse = VideoSubmittedResponse
  { videoId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON VideoSubmittedResponse where
  toJSON v = object ["id" .= videoId v]

instance FromJSON VideoSubmittedResponse where
  parseJSON = withObject "VideoSubmittedResponse" $ \o -> VideoSubmittedResponse <$> o .: "id"

-- ── Guest photos & videos (live gallery) ─────────────────────────────────────

data MediaKind = MediaPhoto | MediaVideo
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MediaKind where
  toJSON MediaPhoto = "photo"
  toJSON MediaVideo = "video"

instance FromJSON MediaKind where
  parseJSON = withText "MediaKind" $ \value ->
    case value of
      "photo" -> pure MediaPhoto
      "video" -> pure MediaVideo
      _       -> fail "MediaKind must be photo or video"

-- | Start a chunked upload: the client declares the file up front.
data MediaUploadInit = MediaUploadInit
  { muiFilename     :: Text
  , muiContentType  :: Text
  , muiSize         :: Int64
  , muiUploaderName :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON MediaUploadInit where
  toJSON m = object
    [ "filename"     .= muiFilename m
    , "contentType"  .= muiContentType m
    , "size"         .= muiSize m
    , "uploaderName" .= muiUploaderName m
    ]

instance FromJSON MediaUploadInit where
  parseJSON = withObject "MediaUploadInit" $ \o ->
    MediaUploadInit
      <$> o .:  "filename"
      <*> o .:? "contentType" .!= ""
      <*> o .:  "size"
      <*> o .:? "uploaderName"

data MediaUploadStarted = MediaUploadStarted
  { musId        :: Text
  , musChunkSize :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON MediaUploadStarted where
  toJSON m = object ["id" .= musId m, "chunkSize" .= musChunkSize m]

instance FromJSON MediaUploadStarted where
  parseJSON = withObject "MediaUploadStarted" $ \o ->
    MediaUploadStarted <$> o .: "id" <*> o .: "chunkSize"

-- | Bytes the server holds for an upload; the client always resumes from
-- 'mupReceived'.
data MediaUploadProgress = MediaUploadProgress
  { mupReceived :: Int64
  , mupComplete :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON MediaUploadProgress where
  toJSON m = object ["received" .= mupReceived m, "complete" .= mupComplete m]

instance FromJSON MediaUploadProgress where
  parseJSON = withObject "MediaUploadProgress" $ \o ->
    MediaUploadProgress <$> o .: "received" <*> o .: "complete"

-- | One published item in the public gallery.
data MediaItem = MediaItem
  { miId           :: Text
  , miKind         :: MediaKind
  , miThumbUrl     :: Text
  , miDisplayUrl   :: Text
  , miVideoUrl     :: Maybe Text
  , miWidth        :: Maybe Int
  , miHeight       :: Maybe Int
  , miDurationMs   :: Maybe Int64
  , miUploaderName :: Maybe Text
  , miReadyAtMs    :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON MediaItem where
  toJSON m = object
    [ "id"           .= miId m
    , "kind"         .= miKind m
    , "thumbUrl"     .= miThumbUrl m
    , "displayUrl"   .= miDisplayUrl m
    , "videoUrl"     .= miVideoUrl m
    , "width"        .= miWidth m
    , "height"       .= miHeight m
    , "durationMs"   .= miDurationMs m
    , "uploaderName" .= miUploaderName m
    , "readyAtMs"    .= miReadyAtMs m
    ]

instance FromJSON MediaItem where
  parseJSON = withObject "MediaItem" $ \o ->
    MediaItem
      <$> o .:  "id"
      <*> o .:  "kind"
      <*> o .:  "thumbUrl"
      <*> o .:  "displayUrl"
      <*> o .:? "videoUrl"
      <*> o .:? "width"
      <*> o .:? "height"
      <*> o .:? "durationMs"
      <*> o .:? "uploaderName"
      <*> o .:  "readyAtMs"

data MediaAdmin = MediaAdmin
  { maId               :: Text
  , maKind             :: MediaKind
  , maOriginalFilename :: Text
  , maContentType      :: Text
  , maSizeBytes        :: Int64
  , maStatus           :: Text
  , maHidden           :: Bool
  , maUploaderName     :: Maybe Text
  , maIpAddress        :: Maybe Text
  , maThumbUrl         :: Maybe Text
  , maCreatedAt        :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON MediaAdmin where
  toJSON m = object
    [ "id"               .= maId m
    , "kind"             .= maKind m
    , "originalFilename" .= maOriginalFilename m
    , "contentType"      .= maContentType m
    , "sizeBytes"        .= maSizeBytes m
    , "status"           .= maStatus m
    , "hidden"           .= maHidden m
    , "uploaderName"     .= maUploaderName m
    , "ipAddress"        .= maIpAddress m
    , "thumbUrl"         .= maThumbUrl m
    , "createdAt"        .= maCreatedAt m
    ]

instance FromJSON MediaAdmin where
  parseJSON = withObject "MediaAdmin" $ \o ->
    MediaAdmin
      <$> o .:  "id"
      <*> o .:  "kind"
      <*> o .:  "originalFilename"
      <*> o .:  "contentType"
      <*> o .:  "sizeBytes"
      <*> o .:  "status"
      <*> o .:  "hidden"
      <*> o .:? "uploaderName"
      <*> o .:? "ipAddress"
      <*> o .:? "thumbUrl"
      <*> o .:  "createdAt"

newtype MediaHiddenBody = MediaHiddenBody
  { mediaHidden :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON MediaHiddenBody where
  toJSON b = object ["hidden" .= mediaHidden b]

instance FromJSON MediaHiddenBody where
  parseJSON = withObject "MediaHiddenBody" $ \o -> MediaHiddenBody <$> o .: "hidden"
