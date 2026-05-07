{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Api
  ( AppConfig (..)
  , app
  ) where

import           Control.Applicative        ((<|>))
import           Control.Concurrent.MVar    (MVar, withMVar)
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as LBC8
import           Data.Char                  (isAlphaNum, ord)
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai                (Application)
import           Servant
import           Servant.Multipart          (MultipartData, MultipartForm, Tmp)
import           System.Directory           (doesFileExist, removeFile)
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import           System.IO                  (hClose, hPutStrLn, stderr)
import           System.IO.Temp             (withSystemTempFile)
import           System.Process             (readProcessWithExitCode)

import           Auth                       (generateToken, verifyPassword)
import qualified Db
import           Upload                     (SavedVideo (..), saveVideoUpload)
import           Wedding.Types              (InviteLookup, Invitee (..), InviteeInput (..),
                                             LinkInviteeBody, LoginRequest (..),
                                             RsvpLoginRequest (..),
                                             RsvpAdmin, RsvpRequest (..),
                                             VideoAdmin (..),
                                             VideoSubmittedResponse (..))

data AppConfig = AppConfig
  { appAdminPasswordHash :: ByteString
  , appVideoDir          :: FilePath
  , appVideoMaxBytes     :: Integer
  , appCookieSecure      :: Bool
  , appQrencodeBin       :: FilePath
  , appPublicBaseUrl     :: Text
  }

type ConnVar = MVar Connection
type CookieHeader = Header "Cookie" Text
type SetCookie a = Headers '[Header "Set-Cookie" Text] a
type DownloadFile = Headers '[Header "Content-Disposition" Text] BL.ByteString

type API =
       "api" :> "health" :> Get '[PlainText] String
  :<|> "api" :> "invite" :> QueryParam "code" Text :> Get '[JSON] InviteLookup
  :<|> "api" :> "rsvp"   :> ReqBody '[JSON] RsvpRequest :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "rsvp" :> "login" :> ReqBody '[JSON] RsvpLoginRequest :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "rsvp" :> "me" :> CookieHeader :> Get '[JSON] NoContent
  :<|> "api" :> "videos" :> CookieHeader :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] VideoSubmittedResponse
  :<|> "api" :> "admin" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "admin" :> "logout" :> CookieHeader :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "admin" :> "me" :> CookieHeader :> Get '[JSON] NoContent
  :<|> "api" :> "admin" :> "invitees" :> CookieHeader :> Get '[JSON] [Invitee]
  :<|> "api" :> "admin" :> "invitees" :> CookieHeader :> ReqBody '[JSON] InviteeInput :> Post '[JSON] Invitee
  :<|> "api" :> "admin" :> "invitees" :> Capture "id" Int64 :> CookieHeader :> ReqBody '[JSON] InviteeInput :> Put '[JSON] Invitee
  :<|> "api" :> "admin" :> "invitees" :> Capture "id" Int64 :> CookieHeader :> Delete '[JSON] NoContent
  :<|> "api" :> "admin" :> "invitees" :> Capture "id" Int64 :> "qr" :> CookieHeader :> Get '[OctetStream] DownloadFile
  :<|> "api" :> "admin" :> "rsvps" :> CookieHeader :> Get '[JSON] [RsvpAdmin]
  :<|> "api" :> "admin" :> "rsvps" :> Capture "id" Text :> "invitee" :> CookieHeader :> ReqBody '[JSON] LinkInviteeBody :> Put '[JSON] RsvpAdmin
  :<|> "api" :> "admin" :> "rsvps" :> Capture "id" Text :> CookieHeader :> Delete '[JSON] NoContent
  :<|> "api" :> "admin" :> "videos" :> CookieHeader :> Get '[JSON] [VideoAdmin]
  :<|> "api" :> "admin" :> "videos" :> Capture "id" Text :> "download" :> CookieHeader :> Get '[OctetStream] DownloadFile
  :<|> "api" :> "admin" :> "videos" :> Capture "id" Text :> CookieHeader :> Delete '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: AppConfig -> ConnVar -> Server API
server cfg var =
       healthH
  :<|> inviteH var
  :<|> rsvpH var
  :<|> rsvpLoginH var
  :<|> rsvpMeH var
  :<|> videoH cfg var
  :<|> loginH cfg var
  :<|> logoutH cfg var
  :<|> meH var
  :<|> listInviteesH var
  :<|> createInviteeH var
  :<|> updateInviteeH var
  :<|> deleteInviteeH var
  :<|> inviteeQrH cfg var
  :<|> listRsvpsH var
  :<|> linkRsvpInviteeH var
  :<|> deleteRsvpH var
  :<|> listVideosH var
  :<|> downloadVideoH cfg var
  :<|> deleteVideoH cfg var

healthH :: Handler String
healthH = pure "ok"

inviteH :: ConnVar -> Maybe Text -> Handler InviteLookup
inviteH var mCode = do
  code <- maybe (throwError err400 { errBody = "\"Missing invitation code\"" }) pure mCode
  mInvite <- withDb var (`Db.lookupInvite` code)
  maybe (throwError err404 { errBody = "\"Invitation not found\"" }) pure mInvite

rsvpH :: ConnVar -> RsvpRequest -> Handler (SetCookie NoContent)
rsvpH var r = do
  result <- withDb var (`Db.submitRsvpRequest` r)
  case result of
    Left msg -> throwError err400 { errBody = textBody msg }
    Right submitted -> do
      token <- liftIO generateToken
      let miid = inviteeId <$> Db.submittedInvitee submitted
      withDb var (\conn -> Db.insertRsvpSession conn token miid (Just (Db.submittedRsvpId submitted)))
      pure (addHeader (rsvpSessionCookie token) NoContent)

rsvpLoginH :: ConnVar -> RsvpLoginRequest -> Handler (SetCookie NoContent)
rsvpLoginH var req = do
  mInvitee <- withDb var (`Db.getInviteeByCode` rsvpLoginCode req)
  invitee <- maybe (throwError err400 { errBody = textBody "Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto." }) pure mInvitee
  token <- liftIO generateToken
  withDb var (\conn -> Db.insertRsvpSession conn token (Just (inviteeId invitee)) Nothing)
  pure (addHeader (rsvpSessionCookie token) NoContent)

rsvpMeH :: ConnVar -> Maybe Text -> Handler NoContent
rsvpMeH var mCookie = do
  _ <- requireRsvp var mCookie
  pure NoContent

videoH :: AppConfig -> ConnVar -> Maybe Text -> MultipartData Tmp -> Handler VideoSubmittedResponse
videoH cfg var mCookie multipart = do
  session <- requireRsvp var mCookie
  saved <- liftIO $ saveVideoUpload (appVideoDir cfg) (appVideoMaxBytes cfg) multipart
  case saved of
    Left msg -> throwError err400 { errBody = textBody msg }
    Right video -> do
      let attributedVideo = video { savedSubmitterName = sessionDisplayName session }
      vid <- withDb var (\conn -> Db.insertVideo conn session attributedVideo)
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
  case extractCookie adminCookieName mCookie of
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
  inputWithCode <- ensureInviteeCode input
  withDb var (`Db.createInvitee` inputWithCode)

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

inviteeQrH :: AppConfig -> ConnVar -> Int64 -> Maybe Text -> Handler DownloadFile
inviteeQrH cfg var iid mCookie = do
  requireAdmin var mCookie
  mInvitee <- withDb var (`Db.getInvitee` iid)
  invitee <- maybe (throwError err404) pure mInvitee
  code <- maybe (throwError err404) pure (inviteeCode invitee >>= nonEmpty)
  bytes <- liftIO $ qrPng (appQrencodeBin cfg) (inviteeUrl cfg code)
  pure (addHeader "inline; filename=invitee-rsvp-qr.png" bytes)

listRsvpsH :: ConnVar -> Maybe Text -> Handler [RsvpAdmin]
listRsvpsH var mCookie = do
  requireAdmin var mCookie
  withDb var Db.listRsvps

linkRsvpInviteeH :: ConnVar -> Text -> Maybe Text -> LinkInviteeBody -> Handler RsvpAdmin
linkRsvpInviteeH var rid mCookie body = do
  requireAdmin var mCookie
  mRsvp <- withDb var (\conn -> Db.linkRsvpInvitee conn rid body)
  maybe (throwError err404) pure mRsvp

deleteRsvpH :: ConnVar -> Text -> Maybe Text -> Handler NoContent
deleteRsvpH var rid mCookie = do
  requireAdmin var mCookie
  withDb var (`Db.deleteRsvp` rid)
  pure NoContent

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

deleteVideoH :: AppConfig -> ConnVar -> Text -> Maybe Text -> Handler NoContent
deleteVideoH cfg var vid mCookie = do
  requireAdmin var mCookie
  mStored <- withDb var (`Db.deleteVideo` vid)
  case mStored of
    Nothing -> throwError err404
    Just stored -> do
      let path = appVideoDir cfg </> T.unpack stored
      _ <- liftIO (try (removeFile path) :: IO (Either SomeException ()))
      pure NoContent

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
  case extractCookie adminCookieName mCookie of
    Nothing -> throwError err401
    Just token -> do
      valid <- withDb var (`Db.lookupSession` token)
      if valid then pure () else throwError err401

requireRsvp :: ConnVar -> Maybe Text -> Handler Db.RsvpSession
requireRsvp var mCookie =
  case extractCookie rsvpCookieName mCookie of
    Nothing -> throwError err401
    Just token -> do
      mInvitee <- withDb var (`Db.lookupRsvpSession` token)
      maybe (throwError err401) pure mInvitee

sessionDisplayName :: Db.RsvpSession -> Maybe Text
sessionDisplayName session = Db.sessionInviteeName session <|> Db.sessionRsvpName session

extractCookie :: Text -> Maybe Text -> Maybe Text
extractCookie name mCookie = do
  cookie <- mCookie
  let parts = T.splitOn ";" cookie
      keyValue part =
        let (key, rest) = T.breakOn "=" (T.strip part)
         in (key, T.drop 1 rest)
  lookup name (map keyValue parts) >>= nonEmpty

sessionCookie :: Bool -> Text -> Text
sessionCookie secure token =
  adminCookieName <> "=" <> token <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=86400" <> secureAttr secure

rsvpSessionCookie :: Text -> Text
rsvpSessionCookie token =
  rsvpCookieName <> "=" <> token <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=15552000"

clearNoopCookie :: Text
clearNoopCookie = "wedding_rsvp_noop=1; Path=/; Max-Age=0"

clearSessionCookie :: Bool -> Text
clearSessionCookie secure =
  adminCookieName <> "=deleted; Path=/; HttpOnly; SameSite=Lax; Max-Age=0; Expires=Thu, 01 Jan 1970 00:00:00 GMT" <> secureAttr secure

secureAttr :: Bool -> Text
secureAttr secure = if secure then "; Secure" else ""

adminCookieName :: Text
adminCookieName = "wedding_admin"

rsvpCookieName :: Text
rsvpCookieName = "wedding_rsvp"

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

ensureInviteeCode :: InviteeInput -> Handler InviteeInput
ensureInviteeCode input =
  case iiCode input >>= nonEmpty of
    Just code -> pure input { iiCode = Just code }
    Nothing -> do
      token <- liftIO generateToken
      pure input { iiCode = Just ("INV-" <> T.filter (/= '-') token) }

inviteeUrl :: AppConfig -> Text -> Text
inviteeUrl cfg code = appPublicBaseUrl cfg <> "/?code=" <> urlEncode code

urlEncode :: Text -> Text
urlEncode = T.concatMap encodeChar
  where
    encodeChar c
      | isAlphaNum c || c `elem` ("-_.~" :: String) = T.singleton c
      | otherwise = T.pack ['%', hex (ord c `div` 16), hex (ord c `mod` 16)]
    hex n = "0123456789ABCDEF" !! n

qrPng :: FilePath -> Text -> IO BL.ByteString
qrPng qrencode value = withSystemTempFile "wedding-invitee-qr.png" $ \path handle -> do
  hClose handle
  (code, _out, err) <- readProcessWithExitCode qrencode
    ["-t", "PNG", "-s", "6", "-m", "2", "-o", path, T.unpack value]
    ""
  case code of
    ExitSuccess   -> BL.readFile path
    ExitFailure _ -> fail ("qrencode failed: " <> err)

app :: AppConfig -> ConnVar -> Application
app cfg var = serve api (server cfg var)
