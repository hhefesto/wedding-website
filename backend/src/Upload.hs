{-# LANGUAGE OverloadedStrings #-}

module Upload
  ( SavedVideo (..)
  , saveVideoUpload
  ) where

import           Control.Monad       (when)
import           Data.Char           (isAlphaNum)
import           Data.Int            (Int64)
import           Data.List           (find)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import           Data.UUID           (toText)
import           Data.UUID.V4        (nextRandom)
import           Servant.Multipart   (FileData (..), Input (..), MultipartData (..), Tmp)
import           System.Directory    (copyFile, createDirectoryIfMissing, getFileSize)
import           System.FilePath     (takeExtension, takeFileName, (</>))

data SavedVideo = SavedVideo
  { savedOriginalFilename :: Text
  , savedStoredFilename   :: Text
  , savedContentType      :: Text
  , savedSizeBytes        :: Int64
  , savedSubmitterName    :: Maybe Text
  , savedMessage          :: Maybe Text
  } deriving (Eq, Show)

saveVideoUpload :: FilePath -> Integer -> MultipartData Tmp -> IO (Either Text SavedVideo)
saveVideoUpload dir maxBytes multipart =
  case files multipart of
    []     -> pure (Left "No video file was submitted.")
    (f:_)  -> do
      let contentType = fdFileCType f
      if not ("video/" `T.isPrefixOf` contentType)
        then pure (Left "Only video uploads are accepted.")
        else do
          size <- getFileSize (fdPayload f)
          if size <= 0 || size > maxBytes
            then pure (Left "Video file is empty or exceeds the configured size limit.")
            else do
              createDirectoryIfMissing True dir
              uuid <- nextRandom
              let original = safeOriginalName (fdFileName f)
                  ext = safeExtension original
                  stored = toText uuid <> ext
                  target = dir </> T.unpack stored
              copyFile (fdPayload f) target
              pure $ Right SavedVideo
                { savedOriginalFilename = original
                , savedStoredFilename   = stored
                , savedContentType      = contentType
                , savedSizeBytes        = fromIntegral size
                , savedSubmitterName    = nonEmpty =<< inputValue "name" multipart
                , savedMessage          = nonEmpty =<< inputValue "message" multipart
                }

inputValue :: Text -> MultipartData Tmp -> Maybe Text
inputValue key multipart =
  iValue <$> find ((== key) . iName) (inputs multipart)

nonEmpty :: Text -> Maybe Text
nonEmpty t =
  let stripped = T.strip t
   in if T.null stripped then Nothing else Just stripped

safeOriginalName :: Text -> Text
safeOriginalName name =
  let base = T.pack (takeFileName (T.unpack name))
      clean = T.map (\c -> if isAllowed c then c else '_') base
   in fromMaybe "video" (nonEmpty clean)

safeExtension :: Text -> Text
safeExtension name =
  let ext = T.pack (takeExtension (T.unpack name))
   in if T.null ext || T.length ext > 12 || T.any (not . isAllowed) ext
        then ".bin"
        else ext

isAllowed :: Char -> Bool
isAllowed c = isAlphaNum c || c `elem` ("._-" :: String)
