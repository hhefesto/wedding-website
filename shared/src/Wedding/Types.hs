module Wedding.Types
  ( Rsvp (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Rsvp = Rsvp
  { name       :: Text
  , guestCount :: Int
  , dietary    :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Rsvp
instance FromJSON Rsvp
