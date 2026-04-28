module Wedding.Types
  ( Rsvp (..)
  , RsvpRequest (..)
  , Invitee (..)
  , InviteeInput (..)
  , LoginRequest (..)
  , RsvpAdmin (..)
  , VideoAdmin (..)
  , LinkInviteeBody (..)
  , VideoSubmittedResponse (..)
  ) where

import           Data.Aeson   (FromJSON (..), ToJSON (..), object, withObject,
                                (.:), (.:?), (.!=), (.=))
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

data RsvpRequest = RsvpRequest
  { rrName           :: Text
  , rrGuestCount     :: Int
  , rrDietary        :: Text
  , rrInvitationCode :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON RsvpRequest where
  toJSON r = object
    [ "name"           .= rrName r
    , "guestCount"     .= rrGuestCount r
    , "dietary"        .= rrDietary r
    , "invitationCode" .= rrInvitationCode r
    ]

instance FromJSON RsvpRequest where
  parseJSON = withObject "RsvpRequest" $ \o ->
    RsvpRequest
      <$> o .:  "name"
      <*> o .:  "guestCount"
      <*> o .:? "dietary" .!= ""
      <*> o .:? "invitationCode"

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

data RsvpAdmin = RsvpAdmin
  { raId                 :: Text
  , raName               :: Text
  , raGuestCount         :: Int
  , raDietary            :: Maybe Text
  , raInviteeId          :: Maybe Int64
  , raInvitationCodeUsed :: Maybe Text
  , raCreatedAt          :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON RsvpAdmin where
  toJSON r = object
    [ "id"                 .= raId r
    , "name"               .= raName r
    , "guestCount"         .= raGuestCount r
    , "dietary"            .= raDietary r
    , "inviteeId"          .= raInviteeId r
    , "invitationCodeUsed" .= raInvitationCodeUsed r
    , "createdAt"          .= raCreatedAt r
    ]

newtype LinkInviteeBody = LinkInviteeBody
  { linkInviteeId :: Maybe Int64
  } deriving (Eq, Show, Generic)

instance FromJSON LinkInviteeBody where
  parseJSON = withObject "LinkInviteeBody" $ \o -> LinkInviteeBody <$> o .:? "inviteeId"

data VideoAdmin = VideoAdmin
  { vaId               :: Text
  , vaOriginalFilename :: Text
  , vaStoredFilename   :: Text
  , vaContentType      :: Text
  , vaSizeBytes        :: Int64
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
    , "submitterName"    .= vaSubmitterName v
    , "message"          .= vaMessage v
    , "createdAt"        .= vaCreatedAt v
    ]

newtype VideoSubmittedResponse = VideoSubmittedResponse
  { videoId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON VideoSubmittedResponse where
  toJSON v = object ["id" .= videoId v]
