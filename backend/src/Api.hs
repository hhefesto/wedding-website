{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Api
  ( AppConfig (..)
  , app
  ) where

import           Control.Concurrent.MVar    (MVar, withMVar)
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as LBC8
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai                (Application)
import           Servant
import           Servant.Multipart          (MultipartData, MultipartForm, Tmp)
import           System.Directory           (doesFileExist)
import           System.FilePath            ((</>))
import           System.IO                  (hPutStrLn, stderr)

import           Auth                       (generateToken, verifyPassword)
import qualified Db
import           Upload                     (saveVideoUpload)
import           Wedding.Types              (Invitee, InviteeInput,
                                             LinkInviteeBody, LoginRequest (..),
                                             RsvpAdmin, RsvpRequest,
                                             VideoAdmin (..),
                                             VideoSubmittedResponse (..))

data AppConfig = AppConfig
  { appAdminPasswordHash :: ByteString
  , appVideoDir          :: FilePath
  , appVideoMaxBytes     :: Integer
  , appCookieSecure      :: Bool
  }

type ConnVar = MVar Connection
type CookieHeader = Header "Cookie" Text
type SetCookie a = Headers '[Header "Set-Cookie" Text] a
type DownloadFile = Headers '[Header "Content-Disposition" Text] BL.ByteString

type API =
       "api" :> "health" :> Get '[PlainText] String
  :<|> "api" :> "rsvp"   :> ReqBody '[JSON] RsvpRequest :> Post '[JSON] NoContent
  :<|> "api" :> "videos" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] VideoSubmittedResponse
  :<|> "api" :> "admin" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "admin" :> "logout" :> CookieHeader :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "admin" :> "me" :> CookieHeader :> Get '[JSON] NoContent
  :<|> "api" :> "admin" :> "invitees" :> CookieHeader :> Get '[JSON] [Invitee]
  :<|> "api" :> "admin" :> "invitees" :> CookieHeader :> ReqBody '[JSON] InviteeInput :> Post '[JSON] Invitee
  :<|> "api" :> "admin" :> "invitees" :> Capture "id" Int64 :> CookieHeader :> ReqBody '[JSON] InviteeInput :> Put '[JSON] Invitee
  :<|> "api" :> "admin" :> "invitees" :> Capture "id" Int64 :> CookieHeader :> Delete '[JSON] NoContent
  :<|> "api" :> "admin" :> "rsvps" :> CookieHeader :> Get '[JSON] [RsvpAdmin]
  :<|> "api" :> "admin" :> "rsvps" :> Capture "id" Text :> "invitee" :> CookieHeader :> ReqBody '[JSON] LinkInviteeBody :> Put '[JSON] RsvpAdmin
  :<|> "api" :> "admin" :> "videos" :> CookieHeader :> Get '[JSON] [VideoAdmin]
  :<|> "api" :> "admin" :> "videos" :> Capture "id" Text :> "download" :> CookieHeader :> Get '[OctetStream] DownloadFile

api :: Proxy API
api = Proxy

server :: AppConfig -> ConnVar -> Server API
server cfg var =
       healthH
  :<|> rsvpH var
  :<|> videoH cfg var
  :<|> loginH cfg var
  :<|> logoutH cfg var
  :<|> meH var
  :<|> listInviteesH var
  :<|> createInviteeH var
  :<|> updateInviteeH var
  :<|> deleteInviteeH var
  :<|> listRsvpsH var
  :<|> linkRsvpInviteeH var
  :<|> listVideosH var
  :<|> downloadVideoH cfg var

healthH :: Handler String
healthH = pure "ok"

rsvpH :: ConnVar -> RsvpRequest -> Handler NoContent
rsvpH var r = do
  withDb var (`Db.insertRsvpRequest` r)
  pure NoContent

videoH :: AppConfig -> ConnVar -> MultipartData Tmp -> Handler VideoSubmittedResponse
videoH cfg var multipart = do
  saved <- liftIO $ saveVideoUpload (appVideoDir cfg) (appVideoMaxBytes cfg) multipart
  case saved of
    Left msg -> throwError err400 { errBody = textBody msg }
    Right video -> do
      vid <- withDb var (`Db.insertVideo` video)
      pure (VideoSubmittedResponse vid)

loginH :: AppConfig -> ConnVar -> LoginRequest -> Handler (SetCookie NoContent)
loginH cfg var req =
  if verifyPassword (appAdminPasswordHash cfg) (loginPassword req)
    then do
      token <- liftIO generateToken
      withDb var (`Db.insertSession` token)
      pure (addHeader (sessionCookie (appCookieSecure cfg) token) NoContent)
    else throwError err401 { errBody = "\"Invalid password\"" }

logoutH :: AppConfig -> ConnVar -> Maybe Text -> Handler (SetCookie NoContent)
logoutH cfg var mCookie = do
  case extractSessionToken mCookie of
    Nothing    -> pure ()
    Just token -> withDb var (`Db.deleteSession` token)
  pure (addHeader (clearSessionCookie (appCookieSecure cfg)) NoContent)

meH :: ConnVar -> Maybe Text -> Handler NoContent
meH var mCookie = do
  requireAdmin var mCookie
  pure NoContent

listInviteesH :: ConnVar -> Maybe Text -> Handler [Invitee]
listInviteesH var mCookie = do
  requireAdmin var mCookie
  withDb var Db.listInvitees

createInviteeH :: ConnVar -> Maybe Text -> InviteeInput -> Handler Invitee
createInviteeH var mCookie input = do
  requireAdmin var mCookie
  withDb var (`Db.createInvitee` input)

updateInviteeH :: ConnVar -> Int64 -> Maybe Text -> InviteeInput -> Handler Invitee
updateInviteeH var iid mCookie input = do
  requireAdmin var mCookie
  mInvitee <- withDb var (\conn -> Db.updateInvitee conn iid input)
  maybe (throwError err404) pure mInvitee

deleteInviteeH :: ConnVar -> Int64 -> Maybe Text -> Handler NoContent
deleteInviteeH var iid mCookie = do
  requireAdmin var mCookie
  withDb var (`Db.deleteInvitee` iid)
  pure NoContent

listRsvpsH :: ConnVar -> Maybe Text -> Handler [RsvpAdmin]
listRsvpsH var mCookie = do
  requireAdmin var mCookie
  withDb var Db.listRsvps

linkRsvpInviteeH :: ConnVar -> Text -> Maybe Text -> LinkInviteeBody -> Handler RsvpAdmin
linkRsvpInviteeH var rid mCookie body = do
  requireAdmin var mCookie
  mRsvp <- withDb var (\conn -> Db.linkRsvpInvitee conn rid body)
  maybe (throwError err404) pure mRsvp

listVideosH :: ConnVar -> Maybe Text -> Handler [VideoAdmin]
listVideosH var mCookie = do
  requireAdmin var mCookie
  withDb var Db.listVideos

downloadVideoH :: AppConfig -> ConnVar -> Text -> Maybe Text -> Handler DownloadFile
downloadVideoH cfg var vid mCookie = do
  requireAdmin var mCookie
  mVideo <- withDb var (`Db.getVideo` vid)
  video <- maybe (throwError err404) pure mVideo
  let path = appVideoDir cfg </> T.unpack (vaStoredFilename video)
  exists <- liftIO (doesFileExist path)
  if not exists
    then throwError err404
    else do
      bytes <- liftIO (BL.readFile path)
      pure (addHeader (downloadDisposition (vaOriginalFilename video)) bytes)

withDb :: ConnVar -> (Connection -> IO a) -> Handler a
withDb var action = do
  result <- liftIO $ try $ withMVar var action
  case result of
    Left (e :: SomeException) -> do
      liftIO $ hPutStrLn stderr $ "DB error: " <> show e
      throwError err500 { errBody = "\"Internal server error\"" }
    Right value -> pure value

requireAdmin :: ConnVar -> Maybe Text -> Handler ()
requireAdmin var mCookie =
  case extractSessionToken mCookie of
    Nothing -> throwError err401
    Just token -> do
      valid <- withDb var (`Db.lookupSession` token)
      if valid then pure () else throwError err401

extractSessionToken :: Maybe Text -> Maybe Text
extractSessionToken mCookie = do
  cookie <- mCookie
  let parts = T.splitOn ";" cookie
      keyValue part =
        let (key, rest) = T.breakOn "=" (T.strip part)
         in (key, T.drop 1 rest)
  lookup cookieName (map keyValue parts) >>= nonEmpty

sessionCookie :: Bool -> Text -> Text
sessionCookie secure token =
  cookieName <> "=" <> token <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=86400" <> secureAttr secure

clearSessionCookie :: Bool -> Text
clearSessionCookie secure =
  cookieName <> "=deleted; Path=/; HttpOnly; SameSite=Lax; Max-Age=0; Expires=Thu, 01 Jan 1970 00:00:00 GMT" <> secureAttr secure

secureAttr :: Bool -> Text
secureAttr secure = if secure then "; Secure" else ""

cookieName :: Text
cookieName = "wedding_admin"

textBody :: Text -> LBC8.ByteString
textBody = LBC8.pack . T.unpack

downloadDisposition :: Text -> Text
downloadDisposition filename = "attachment; filename=\"" <> T.map safeDispositionChar filename <> "\""

safeDispositionChar :: Char -> Char
safeDispositionChar c
  | c == '"' || c == '\\' || c == '\r' || c == '\n' = '_'
  | otherwise = c

nonEmpty :: Text -> Maybe Text
nonEmpty value =
  let stripped = T.strip value
   in if T.null stripped then Nothing else Just stripped

app :: AppConfig -> ConnVar -> Application
app cfg var = serve api (server cfg var)
