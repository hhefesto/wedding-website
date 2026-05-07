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
  , LinkInviteeBody (..)
  , VideoSubmittedResponse (..)
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
      <*> o .:  "createdAt"

newtype LinkInviteeBody = LinkInviteeBody
  { linkInviteeId :: Maybe Int64
  } deriving (Eq, Show, Generic)

instance FromJSON LinkInviteeBody where
  parseJSON = withObject "LinkInviteeBody" $ \o -> LinkInviteeBody <$> o .:? "inviteeId"

instance ToJSON LinkInviteeBody where
  toJSON body = object ["inviteeId" .= linkInviteeId body]

data VideoAdmin = VideoAdmin
  { vaId               :: Text
  , vaOriginalFilename :: Text
  , vaStoredFilename   :: Text
  , vaContentType      :: Text
  , vaSizeBytes        :: Int64
  , vaInviteeId        :: Maybe Int64
  , vaSubmitterName    :: Maybe Text
  , vaMessage          :: Maybe Text
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
    , "submitterName"    .= vaSubmitterName v
    , "message"          .= vaMessage v
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
      <*> o .:? "submitterName"
      <*> o .:? "message"
      <*> o .:  "createdAt"

newtype VideoSubmittedResponse = VideoSubmittedResponse
  { videoId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON VideoSubmittedResponse where
  toJSON v = object ["id" .= videoId v]

instance FromJSON VideoSubmittedResponse where
  parseJSON = withObject "VideoSubmittedResponse" $ \o -> VideoSubmittedResponse <$> o .: "id"
