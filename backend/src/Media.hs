{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Guest photo/video uploads for the live gallery.
--
-- Uploads arrive in chunks (Cloudflare caps a single request at 100 MB) and
-- are appended to @incoming/<id>.part@; the part file's size is the source of
-- truth for how much the server holds, so a client can always resume. Once
-- complete the original moves to @originals/@ untouched, and a worker writes
-- public, metadata-stripped derivatives to @public/@:
--
--   * photos: @<id>-thumb.jpg@ and @<id>-display.jpg@ (vipsthumbnail)
--   * videos: @<id>-thumb.jpg@ poster and @<id>-preview.mp4@ (ffmpeg)
--
-- External tools (vipsthumbnail, ffmpeg, ffprobe, df, nice) are resolved via
-- PATH; the NixOS module puts them there.
module Media
  ( MediaConfig (..)
  , MediaRuntime
  , ChunkOutcome (..)
  , newMediaRuntime
  , ensureMediaDirs
  , mediaChunkSize
  , classifyUpload
  , storedNameFor
  , beginUpload
  , receivedBytes
  , originalExists
  , appendChunk
  , enqueueMedia
  , startMediaWorkers
  , publicDir
  , mediaItem
  , adminThumbUrl
  , removeMediaFiles
  , removePartFile
  , freeBytes
  ) where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.MVar     (MVar, withMVar)
import           Control.Concurrent.STM      (TQueue, TVar, atomically, modifyTVar',
                                              newTQueueIO, newTVarIO, readTQueue,
                                              readTVar, writeTQueue, writeTVar)
import           Control.Exception           (SomeException, bracket, try)
import           Control.Monad               (forM_, forever, void, when)
import qualified Data.ByteString.Lazy        as BL
import           Data.Int                    (Int64)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Database.PostgreSQL.Simple  (Connection)
import           System.Directory            (createDirectoryIfMissing, doesFileExist,
                                              getFileSize, removeFile, renameFile)
import           System.Exit                 (ExitCode (..))
import           System.FilePath             ((</>))
import           System.IO                   (IOMode (AppendMode), hPutStrLn, stderr,
                                              withBinaryFile)
import           System.Process              (readProcessWithExitCode)
import           System.Timeout              (timeout)
import           Text.Printf                 (printf)
import           Text.Read                   (readMaybe)

import qualified Db
import           Upload                      (isVideoUpload, safeExtension)
import           Wedding.Types               (MediaItem (..), MediaKind (..))

data MediaConfig = MediaConfig
  { mediaDir          :: FilePath
  , mediaMaxBytes     :: Integer
  , mediaMinFreeBytes :: Integer
  }

data MediaRuntime = MediaRuntime
  { rtInFlight   :: TVar (Set Text)
  , rtPhotoQueue :: TQueue Text
  , rtVideoQueue :: TQueue Text
  }

data ChunkOutcome
  = ChunkStored Int64   -- ^ bytes now held (unchanged when the offset was stale)
  | ChunkFinished       -- ^ last byte arrived; the original is in place
  | ChunkBusy           -- ^ another request is writing this upload right now
  | ChunkMissing        -- ^ no part file: expired, or already finished
  | ChunkTooLarge       -- ^ the chunk would exceed the declared size

newMediaRuntime :: IO MediaRuntime
newMediaRuntime = MediaRuntime <$> newTVarIO Set.empty <*> newTQueueIO <*> newTQueueIO

-- | Well under Cloudflare's 100 MB request cap, small enough that a dropped
-- connection on venue Wi-Fi loses little.
mediaChunkSize :: Int64
mediaChunkSize = 8 * 1024 * 1024

incomingDir, originalsDir, publicDir :: MediaConfig -> FilePath
incomingDir  cfg = mediaDir cfg </> "incoming"
originalsDir cfg = mediaDir cfg </> "originals"
publicDir    cfg = mediaDir cfg </> "public"

ensureMediaDirs :: MediaConfig -> IO ()
ensureMediaDirs cfg =
  mapM_ (createDirectoryIfMissing True) [incomingDir cfg, originalsDir cfg, publicDir cfg]

partPath :: MediaConfig -> Text -> FilePath
partPath cfg mid = incomingDir cfg </> T.unpack mid <> ".part"

classifyUpload :: Text -> Text -> Maybe MediaKind
classifyUpload contentType filename
  | "image/" `T.isPrefixOf` ct                           = Just MediaPhoto
  | T.toLower (safeExtension filename) `elem` photoExts  = Just MediaPhoto
  | isVideoUpload contentType filename                   = Just MediaVideo
  | otherwise                                            = Nothing
  where
    ct = T.toLower contentType
    photoExts = [".jpg", ".jpeg", ".png", ".heic", ".heif", ".webp", ".gif", ".tif", ".tiff", ".dng"]

storedNameFor :: Text -> Text -> Text
storedNameFor mid original = mid <> T.toLower (safeExtension original)

beginUpload :: MediaConfig -> Text -> IO ()
beginUpload cfg mid = BL.writeFile (partPath cfg mid) BL.empty

receivedBytes :: MediaConfig -> Text -> IO (Maybe Int64)
receivedBytes cfg mid = do
  let path = partPath cfg mid
  exists <- doesFileExist path
  if exists then Just . fromIntegral <$> getFileSize path else pure Nothing

originalExists :: MediaConfig -> Text -> IO Bool
originalExists cfg stored = doesFileExist (originalsDir cfg </> T.unpack stored)

-- | Append a chunk if it starts exactly where the part file ends. A stale or
-- repeated offset writes nothing and reports the current size, so retries are
-- idempotent. Only one request per upload may write at a time.
appendChunk :: MediaConfig -> MediaRuntime -> Text -> Text -> Int64 -> Int64 -> BL.ByteString -> IO ChunkOutcome
appendChunk cfg rt mid stored declared offset body = do
  -- Force the whole body before taking the lock, so a slow client never
  -- holds it while bytes trickle in.
  let !len = BL.length body
  bracket acquire release $ \acquired ->
    if not acquired then pure ChunkBusy else do
      let path = partPath cfg mid
      exists <- doesFileExist path
      if not exists then pure ChunkMissing else do
        current <- fromIntegral <$> getFileSize path
        if offset /= current
          then pure (ChunkStored current)
          else if current + len > declared
            then pure ChunkTooLarge
            else do
              withBinaryFile path AppendMode (`BL.hPut` body)
              let held = current + len
              if held == declared
                then do
                  renameFile path (originalsDir cfg </> T.unpack stored)
                  pure ChunkFinished
                else pure (ChunkStored held)
  where
    acquire = atomically $ do
      busy <- readTVar (rtInFlight rt)
      if Set.member mid busy
        then pure False
        else writeTVar (rtInFlight rt) (Set.insert mid busy) >> pure True
    release acquired =
      when acquired $ atomically $ modifyTVar' (rtInFlight rt) (Set.delete mid)

enqueueMedia :: MediaRuntime -> Text -> MediaKind -> IO ()
enqueueMedia rt mid kind = atomically $ writeTQueue (queueFor kind) mid
  where
    queueFor MediaPhoto = rtPhotoQueue rt
    queueFor MediaVideo = rtVideoQueue rt

-- | One worker per kind, so a long transcode never delays photos. Work left
-- in 'processing' by a previous run is picked up again.
startMediaWorkers :: MediaConfig -> MediaRuntime -> MVar Connection -> IO ()
startMediaWorkers cfg rt var = do
  pending <- withMVar var Db.listProcessingMedia
  forM_ pending $ \(mid, kind) -> enqueueMedia rt mid kind
  void $ forkIO $ worker (rtPhotoQueue rt) (2 * 60)
  void $ forkIO $ worker (rtVideoQueue rt) (60 * 60)
  where
    worker queue limitSeconds = forever $ do
      mid <- atomically (readTQueue queue)
      outcome <- try $ do
        mRow <- withMVar var (`Db.getMediaUpload` mid)
        case mRow of
          Nothing  -> pure ()
          Just row -> do
            result <- timeout (limitSeconds * 1000000) (process mid row)
            case result of
              Just (w, h, d) -> withMVar var (\c -> Db.markMediaReady c mid w h d)
              Nothing -> do
                logMedia mid "processing timed out"
                withMVar var (`Db.markMediaFailed` mid)
      case outcome of
        Right () -> pure ()
        Left (e :: SomeException) -> do
          logMedia mid ("processing failed: " <> show e)
          _ <- try (withMVar var (`Db.markMediaFailed` mid)) :: IO (Either SomeException ())
          pure ()

    process mid row =
      let original = originalsDir cfg </> T.unpack (Db.murStored row)
       in case Db.murKind row of
            MediaPhoto -> processPhoto cfg mid original
            MediaVideo -> processVideo cfg mid original

-- | Display and thumbnail JPEGs: auto-rotated by EXIF, converted to sRGB
-- (iPhones shoot Display P3) and stripped of all metadata, GPS included.
processPhoto :: MediaConfig -> Text -> FilePath -> IO (Maybe Int, Maybe Int, Maybe Int64)
processPhoto cfg mid original = do
  let display = publicDir cfg </> displayName mid
      thumb   = publicDir cfg </> thumbName mid
  runTool "vipsthumbnail"
    [original, "--size", "2048x2048>", "--export-profile", "srgb", "-o", display <> "[Q=84,strip]"]
  runTool "vipsthumbnail"
    [original, "--size", "960x960>", "--export-profile", "srgb", "-o", thumb <> "[Q=78,strip]"]
  (w, h) <- probeDimensions display
  pure (w, h, Nothing)

-- | Poster frame plus a 720p-class H.264 preview. yuv420p is required: 10-bit
-- HDR iPhone footage would otherwise become High 10 H.264, which browsers
-- cannot play.
processVideo :: MediaConfig -> Text -> FilePath -> IO (Maybe Int, Maybe Int, Maybe Int64)
processVideo cfg mid original = do
  duration <- probeDuration original
  let thumb   = publicDir cfg </> thumbName mid
      preview = publicDir cfg </> previewName mid
      partial = preview <> ".partial"
      posterAt = maybe 0 (\d -> min 1 (d / 2)) duration :: Double
  runTool "nice"
    [ "-n", "10", "ffmpeg", "-nostdin", "-hide_banner", "-loglevel", "error", "-y"
    , "-ss", printf "%.3f" posterAt, "-i", original
    , "-frames:v", "1"
    , "-vf", "scale=w='min(960,iw)':h='min(960,ih)':force_original_aspect_ratio=decrease"
    , "-q:v", "4", thumb
    ]
  runTool "nice"
    [ "-n", "10", "ffmpeg", "-nostdin", "-hide_banner", "-loglevel", "error", "-y"
    , "-i", original
    , "-map", "0:v:0", "-map", "0:a:0?"
    , "-vf", "scale=w='min(1280,iw)':h='min(1280,ih)':force_original_aspect_ratio=decrease:force_divisible_by=2,format=yuv420p"
    , "-c:v", "libx264", "-preset", "veryfast", "-crf", "27"
    , "-c:a", "aac", "-b:a", "128k", "-ac", "2"
    , "-movflags", "+faststart", "-f", "mp4", partial
    ]
  renameFile partial preview
  (w, h) <- probeDimensions preview
  pure (w, h, round . (* 1000) <$> duration)

probeDimensions :: FilePath -> IO (Maybe Int, Maybe Int)
probeDimensions path = do
  out <- toolOutput "ffprobe"
    [ "-v", "error", "-select_streams", "v:0"
    , "-show_entries", "stream=width,height", "-of", "csv=p=0:s=x", path ]
  pure $ case T.splitOn "x" (T.strip (T.pack out)) of
    [w, h] -> (readMaybe (T.unpack w), readMaybe (T.unpack h))
    _      -> (Nothing, Nothing)

probeDuration :: FilePath -> IO (Maybe Double)
probeDuration path = do
  out <- toolOutput "ffprobe"
    [ "-v", "error", "-show_entries", "format=duration"
    , "-of", "default=noprint_wrappers=1:nokey=1", path ]
  pure (readMaybe (T.unpack (T.strip (T.pack out))))

runTool :: FilePath -> [String] -> IO ()
runTool bin args = void (toolOutput bin args)

toolOutput :: FilePath -> [String] -> IO String
toolOutput bin args = do
  (code, out, err) <- readProcessWithExitCode bin args ""
  case code of
    ExitSuccess   -> pure out
    ExitFailure n -> ioError (userError (bin <> " exited " <> show n <> ": " <> take 2000 err))

-- | Bytes available on the filesystem holding the media directory.
freeBytes :: MediaConfig -> IO (Maybe Integer)
freeBytes cfg = do
  result <- try (toolOutput "df" ["-B1", "--output=avail", mediaDir cfg])
  pure $ case result of
    Left (_ :: SomeException) -> Nothing
    Right out -> case reverse (lines out) of
      (lastLine:_) -> readMaybe (T.unpack (T.strip (T.pack lastLine)))
      []           -> Nothing

thumbName, displayName, previewName :: Text -> FilePath
thumbName   mid = T.unpack mid <> "-thumb.jpg"
displayName mid = T.unpack mid <> "-display.jpg"
previewName mid = T.unpack mid <> "-preview.mp4"

filesUrl :: FilePath -> Text
filesUrl name = "/api/media/files/" <> T.pack name

mediaItem :: (Text, MediaKind, Maybe Int, Maybe Int, Maybe Int64, Maybe Text, Maybe Text, Int64) -> MediaItem
mediaItem (mid, kind, w, h, d, uploader, comment, readyAt) = MediaItem
  { miId           = mid
  , miKind         = kind
  , miThumbUrl     = filesUrl (thumbName mid)
  , miDisplayUrl   = case kind of
      MediaPhoto -> filesUrl (displayName mid)
      MediaVideo -> filesUrl (thumbName mid)
  , miVideoUrl     = case kind of
      MediaPhoto -> Nothing
      MediaVideo -> Just (filesUrl (previewName mid))
  , miWidth        = w
  , miHeight       = h
  , miDurationMs   = d
  , miUploaderName = uploader
  , miComment      = comment
  , miReadyAtMs    = readyAt
  }

adminThumbUrl :: Text -> Text -> Maybe Text
adminThumbUrl mid status
  | status == "ready" = Just (filesUrl (thumbName mid))
  | otherwise         = Nothing

-- | Best-effort removal of everything stored for one item.
removeMediaFiles :: MediaConfig -> Text -> Text -> IO ()
removeMediaFiles cfg mid stored =
  forM_ paths $ \path -> try (removeFile path) :: IO (Either SomeException ())
  where
    paths =
      [ originalsDir cfg </> T.unpack stored
      , partPath cfg mid
      , publicDir cfg </> thumbName mid
      , publicDir cfg </> displayName mid
      , publicDir cfg </> previewName mid
      ]

removePartFile :: MediaConfig -> Text -> IO ()
removePartFile cfg mid = void (try (removeFile (partPath cfg mid)) :: IO (Either SomeException ()))

logMedia :: Text -> String -> IO ()
logMedia mid msg = hPutStrLn stderr ("media " <> T.unpack mid <> ": " <> msg)
