module Api
  ( app
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai                (Application)
import           Servant

import           Db                         (insertRsvp)
import           Wedding.Types              (Rsvp)

type API =
       "api" :> "health" :> Get '[PlainText] String
  :<|> "api" :> "rsvp"   :> ReqBody '[JSON] Rsvp :> Post '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: Connection -> Server API
server conn = healthH :<|> rsvpH
  where
    healthH :: Handler String
    healthH = pure "ok"

    rsvpH :: Rsvp -> Handler NoContent
    rsvpH r = do
      liftIO (insertRsvp conn r)
      pure NoContent

app :: Connection -> Application
app conn = serve api (server conn)
