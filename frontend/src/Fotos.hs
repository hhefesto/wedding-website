{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Guest photos & videos: the QR upload page (@/fotos@), the live gallery
-- carousel shared by the main site and that page, and its lightbox.
--
-- Uploads go to the backend in resumable 8 MiB chunks (Cloudflare caps a
-- request at 100 MB). The byte-pushing loop runs in forked GHCJS threads
-- using ghcjs-dom's XMLHttpRequest directly, because reflex-dom's XHR helper
-- does not expose upload progress.
module Fotos
  ( GalleryItems
  , fotosPage
  , galleryFeed
  , galleryBand
  , fotosCSS
  ) where

import           Control.Concurrent          (forkIO, killThread, threadDelay)
import           Control.Concurrent.QSem     (QSem, newQSem, signalQSem, waitQSem)
import           Control.Exception           (SomeException, bracket_, try)
import           Control.Monad               (forM_, forever, guard, when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy        as BL
import           Data.Int                    (Int64)
import           Data.IORef                  (IORef, atomicModifyIORef', newIORef, readIORef,
                                              writeIORef)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, isJust)
import           Data.Ord                    (Down (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Language.Javascript.JSaddle (JSM, askJSM, liftJSM, runJSM)
import           Reflex.Dom

import qualified GHCJS.DOM                   as DOM
import qualified GHCJS.DOM.BeforeUnloadEvent as BeforeUnload
import qualified GHCJS.DOM.Blob              as Blob
import           GHCJS.DOM.EventM            (event, on)
import qualified GHCJS.DOM.EventM            as EventM
import qualified GHCJS.DOM.Element           as Element
import qualified GHCJS.DOM.File              as File
import qualified GHCJS.DOM.HTMLElement       as HTMLElement
import qualified GHCJS.DOM.HTMLInputElement  as Input
import qualified GHCJS.DOM.ProgressEvent     as Progress
import qualified GHCJS.DOM.Storage           as Storage
import qualified GHCJS.DOM.Types             as DOM
import qualified GHCJS.DOM.Window            as Window
import qualified GHCJS.DOM.WindowEventHandlers as WindowEvents
import qualified GHCJS.DOM.XMLHttpRequest    as Xhr
import qualified GHCJS.DOM.XMLHttpRequestEventTarget as XhrEv

import           Wedding.Types               (MediaItem (..), MediaKind (..),
                                              MediaUploadInit (..),
                                              MediaUploadProgress (..),
                                              MediaUploadStarted (..))

-- ── Gallery data ──────────────────────────────────────────────────────────────

-- | Newest first: keyed by (Down ready-at, id).
type GalleryKey = (Down Int64, Text)
type GalleryItems t = Dynamic t (Map GalleryKey MediaItem)

-- | Polls the published gallery every 15 s (and on demand). The backend
-- returns the whole list each time, so hidden items drop out and videos that
-- finish transcoding late still appear in order.
galleryFeed :: MonadWidget t m => Event t () -> m (GalleryItems t)
galleryFeed refreshE = do
  pb <- getPostBuild
  tickE <- tickLossyFromPostBuildTime 15
  respE <- performRequestAsync $
    XhrRequest "GET" "/api/media" def <$ leftmost [pb, () <$ tickE, refreshE]
  let itemsE = fmapMaybe (decodeXhrResponse :: XhrResponse -> Maybe [MediaItem]) respE
  itemsD <- holdDyn Map.empty (Map.fromList . map (\i -> ((Down (miReadyAtMs i), miId i), i)) <$> itemsE)
  holdUniqDyn itemsD

-- ── Gallery band (carousel + lightbox) ───────────────────────────────────────

galleryBand :: MonadWidget t m => Text -> GalleryItems t -> m ()
galleryBand sectionId itemsD =
  elAttr "section" ("id" =: sectionId <> "class" =: "section galeria") $ do
    elAttr "div" ("class" =: "galeria-head") $ do
      elAttr "p" ("class" =: "galeria-live") $ do
        elAttr "span" ("class" =: "galeria-dot" <> "aria-hidden" =: "true") blank
        text "EN VIVO"
      elAttr "h2" ("class" =: "galeria-title") $ text "Galer\237a de la boda"
      elAttr "p" ("class" =: "galeria-count" <> "aria-live" =: "polite") $
        dynText (countLabel <$> itemsD)
    openE <- carousel itemsD
    lightbox itemsD openE

countLabel :: Map GalleryKey MediaItem -> Text
countLabel m
  | Map.null m = "A\250n no hay fotos"
  | otherwise  = T.intercalate " \183 " (filter (not . T.null) [plural photos "foto" "fotos", plural videos "video" "videos"])
  where
    kinds = map miKind (Map.elems m)
    photos = length (filter (== MediaPhoto) kinds)
    videos = length (filter (== MediaVideo) kinds)
    plural :: Int -> Text -> Text -> Text
    plural 0 _ _ = ""
    plural 1 one _ = "1 " <> one
    plural n _ many = T.pack (show n) <> " " <> many

carousel :: MonadWidget t m => GalleryItems t -> m (Event t GalleryKey)
carousel itemsD =
  elAttr "div" ("class" =: "galeria-viewport") $ do
    (prevEl, _) <- elAttr' "button"
      ("class" =: "galeria-nav galeria-prev" <> "type" =: "button" <> "aria-label" =: "Anteriores") $
      text "\8592"
    (trackEl, openE) <- elAttr' "div"
      ( "class" =: "galeria-track"
     <> "role" =: "region"
     <> "tabindex" =: "0"
     <> "aria-label" =: "Fotos y videos de los invitados"
      ) $ do
      elDynAttr "div" (ffor itemsD $ \m -> "class" =: "galeria-empty" <> if Map.null m then mempty else "hidden" =: "") $ do
        forM_ [1 .. 3 :: Int] $ \_ -> elAttr "div" ("class" =: "galeria-ghost" <> "aria-hidden" =: "true") blank
        elAttr "p" ("class" =: "galeria-empty-copy") $
          text "El 10 \183 10 \183 26 aqu\237 aparecer\225n las fotos y videos de todos."
      selE <- listViewWithKey itemsD galleryTile
      pure (fmapMaybe (fmap fst . Map.lookupMin) selE)
    (nextEl, _) <- elAttr' "button"
      ("class" =: "galeria-nav galeria-next" <> "type" =: "button" <> "aria-label" =: "Siguientes") $
      text "\8594"

    -- Gentle auto-advance while nobody is touching the strip.
    let raw = _element_raw trackEl
        prevE = domEvent Click prevEl
        nextE = domEvent Click nextEl
        interactE = leftmost
          [ () <$ domEvent Touchstart trackEl, () <$ domEvent Mousedown trackEl
          , () <$ domEvent Wheel trackEl, () <$ domEvent Focus trackEl
          , prevE, nextE, () <$ openE ]
    tickE <- tickLossyFromPostBuildTime 5
    hoverD <- holdDyn False $ leftmost [True <$ domEvent Mouseenter trackEl, False <$ domEvent Mouseleave trackEl]
    idleD <- foldDyn ($) (0 :: Int) $ leftmost [const 0 <$ interactE, (+ 1) <$ tickE]
    let autoE = gate (current (zipDynWith (\idle hovering -> idle >= 2 && not hovering) idleD hoverD)) tickE
    performEvent_ $ liftJSM (stepTrack raw True) <$ leftmost [() <$ autoE, nextE]
    performEvent_ $ liftJSM (stepTrack raw False) <$ prevE
    pure openE

-- | Scroll the strip by most of a viewport; forward wraps to the start.
-- The track has scroll-behavior: smooth, so this animates.
stepTrack :: DOM.Element -> Bool -> JSM ()
stepTrack el forward = do
  left <- Element.getScrollLeft el
  width <- Element.getScrollWidth el
  client <- Element.getClientWidth el
  let page = max 1 (round (client * 0.8)) :: Int
      maxLeft = width - round client
      target
        | maxLeft <= 4         = left
        | forward && left >= maxLeft - 4 = 0
        | forward              = min maxLeft (left + page)
        | otherwise            = max 0 (left - page)
  when (target /= left) $ Element.setScrollLeft el target

galleryTile :: MonadWidget t m => GalleryKey -> Dynamic t MediaItem -> m (Event t ())
galleryTile _ itemD = do
  item <- sample (current itemD)
  let isVideo = miKind item == MediaVideo
      ratio = case (miWidth item, miHeight item) of
        (Just w, Just h) | w > 0 && h > 0 -> T.pack (show w) <> " / " <> T.pack (show h)
        _ -> "3 / 4"
      label = (if isVideo then "Ver video" else "Ver foto")
        <> maybe "" (" de " <>) (miUploaderName item)
  (btn, _) <- elAttr' "button"
    ( "class" =: ("galeria-tile" <> if isVideo then " is-video" else "")
   <> "type" =: "button"
   <> "style" =: ("aspect-ratio: " <> ratio)
   <> "aria-label" =: label
    ) $ do
    elAttr "img"
      ( "src" =: miThumbUrl item <> "alt" =: "" <> "loading" =: "lazy"
     <> "decoding" =: "async" <> "draggable" =: "false" ) blank
    when isVideo $ do
      elAttr "span" ("class" =: "galeria-play" <> "aria-hidden" =: "true") blank
      forM_ (miDurationMs item) $ \d ->
        elAttr "span" ("class" =: "galeria-duration") $ text (formatDuration d)
    forM_ (miUploaderName item) $ \n ->
      elAttr "span" ("class" =: "galeria-credit") $ text n
  pure (domEvent Click btn)

formatDuration :: Int64 -> Text
formatDuration ms =
  let total = (ms + 500) `div` 1000
      (m, s) = total `divMod` 60
   in T.pack (show m) <> ":" <> (if s < 10 then "0" else "") <> T.pack (show s)

-- ── Lightbox ──────────────────────────────────────────────────────────────────

lightbox :: MonadWidget t m => GalleryItems t -> Event t GalleryKey -> m ()
lightbox itemsD openE = mdo
  selD <- holdDyn Nothing $ leftmost [Just <$> openE, Nothing <$ closeE, Just <$> moveE]
  let moveE = attachWithMaybe
        (\(m, sel) forward -> do
            k <- sel
            fst <$> (if forward then Map.lookupGT k m else Map.lookupLT k m))
        ((,) <$> current itemsD <*> current selD)
        dirE
  viewE <- dyn $ ffor selD $ \case
    Nothing -> pure (never, never)
    Just k  -> lightboxView itemsD k
  closeE <- switchHold never (fst <$> viewE)
  dirE <- switchHold never (snd <$> viewE)
  pure ()

lightboxView :: MonadWidget t m => GalleryItems t -> GalleryKey -> m (Event t (), Event t Bool)
lightboxView itemsD k = do
  m <- sample (current itemsD)
  case Map.lookup k m of
    Nothing -> do
      pb <- getPostBuild
      pure (pb, never)
    Just item -> elAttr "div" ("class" =: "lightbox" <> "role" =: "dialog" <> "aria-modal" =: "true") $ do
      (backdrop, _) <- elAttr' "div" ("class" =: "lightbox-backdrop") blank
      (frame, (closeB, prevB, nextB)) <- elAttr' "div" ("class" =: "lightbox-frame" <> "tabindex" =: "-1") $ do
        elAttr "div" ("class" =: "lightbox-media") $
          case miVideoUrl item of
            Just url -> elAttr "video"
              ( "class" =: "lightbox-video" <> "src" =: url <> "poster" =: miThumbUrl item
             <> "controls" =: "" <> "playsinline" =: "" <> "autoplay" =: "" <> "preload" =: "metadata" ) blank
            Nothing -> elAttr "img"
              ( "class" =: "lightbox-img" <> "src" =: miDisplayUrl item <> "alt" =: "Foto de la boda" ) blank
        forM_ (miUploaderName item) $ \n ->
          elAttr "p" ("class" =: "lightbox-credit") $ text ("por " <> n)
        (c, _) <- elAttr' "button" ("class" =: "lightbox-btn lightbox-close" <> "type" =: "button" <> "aria-label" =: "Cerrar") $ text "\215"
        (p, _) <- elAttr' "button" ("class" =: "lightbox-btn lightbox-prev" <> "type" =: "button" <> "aria-label" =: "M\225s nueva") $ text "\8592"
        (n, _) <- elAttr' "button" ("class" =: "lightbox-btn lightbox-next" <> "type" =: "button" <> "aria-label" =: "M\225s antigua") $ text "\8594"
        pure (c, p, n)
      pb <- getPostBuild
      performEvent_ $ liftJSM (HTMLElement.focus (DOM.uncheckedCastTo DOM.HTMLElement (_element_raw frame))) <$ pb
      let keyE = domEvent Keydown frame
          closeE = leftmost [domEvent Click closeB, domEvent Click backdrop, () <$ ffilter (== 27) keyE]
          dirE = leftmost
            [ False <$ domEvent Click prevB, True <$ domEvent Click nextB
            , False <$ ffilter (== 37) keyE, True <$ ffilter (== 39) keyE ]
      pure (closeE, dirE)

-- ── Upload page (/fotos) ─────────────────────────────────────────────────────

fotosPage :: MonadWidget t m => m ()
fotosPage =
  elAttr "main" ("class" =: "fotos-page") $ do
    elAttr "header" ("class" =: "fotos-header") $ do
      elAttr "a" ("class" =: "fotos-back" <> "href" =: "/") $ text "\8592 Invitaci\243n"
      elAttr "p" ("class" =: "fotos-kicker") $ text "COMPARTE TUS FOTOS Y VIDEOS"
      elAttr "h1" ("class" =: "fotos-names") $ text "Daniel y Ana Cristina"
      elAttr "p" ("class" =: "fotos-date") $ text "10 \183 10 \183 26"
    doneE <- elAttr "section" ("class" =: "fotos-card") uploader
    -- Processing takes a moment after the last byte; look again shortly.
    soonE <- delay 4 doneE
    laterE <- delay 15 doneE
    itemsD <- galleryFeed (leftmost [soonE, laterE])
    galleryBand "galeria" itemsD

data FileMeta = FileMeta
  { fmFile    :: DOM.File
  , fmName    :: Text
  , fmType    :: Text
  , fmSize    :: Int64
  , fmIsVideo :: Bool
  }

data UpStatus = UpQueued | UpSending Int | UpRetrying | UpDone | UpFailed Text
  deriving (Eq)

data UpEntry = UpEntry
  { ueMeta     :: FileMeta
  , ueStatus   :: UpStatus
  , ueUploadId :: Maybe Text
  }

-- | Compared by everything except the JS File handle, which has no Eq.
instance Eq UpEntry where
  a == b = ueStatus a == ueStatus b
        && ueUploadId a == ueUploadId b
        && fmName (ueMeta a) == fmName (ueMeta b)
        && fmSize (ueMeta a) == fmSize (ueMeta b)

data UpMsg = UpMsg Int UpUpdate

data UpUpdate = SetStatus UpStatus | SetUploadId (Maybe Text)

-- | Name field, file picker, and the live upload queue. Fires whenever a file
-- finishes uploading.
uploader :: MonadWidget t m => m (Event t ())
uploader = mdo
  elAttr "p" ("class" =: "fotos-copy") $
    text "Sube tus fotos y videos en calidad original. Puedes elegir varios a la vez."
  savedName <- liftJSM loadUploaderName
  nameEl <- inputElement $ def
    & inputElementConfig_initialValue .~ savedName
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ( "class" =: "rsvp-input fotos-name" <> "placeholder" =: "Tu nombre (opcional)"
     <> "maxlength" =: "80" <> "autocomplete" =: "name" <> "aria-label" =: "Tu nombre (opcional)" )
  performEvent_ $ liftJSM . saveUploaderName <$> updated (_inputElement_value nameEl)
  fileEl <- elAttr "label" ("class" =: "fotos-pick") $ do
    fi <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("type" =: "file" <> "multiple" =: "" <> "accept" =: "image/*,video/*" <> "class" =: "fotos-file")
    elAttr "span" ("class" =: "fotos-pick-icon" <> "aria-hidden" =: "true") blank
    elAttr "span" ("class" =: "fotos-pick-label") $ text "Elegir fotos y videos"
    pure fi
  elAttr "p" ("class" =: "fotos-hint") $
    text "\191Se\241al lenta? Puedes subirlas despu\233s \8212 este enlace seguir\225 funcionando."

  -- Read the picked files, then clear the input so the same files can be
  -- picked again.
  let pickedE = ffilter (not . null) (updated (_inputElement_files fileEl))
  metasE <- performEvent $ ffor pickedE $ \files -> liftJSM $ do
    metas <- mapM fileMeta files
    Input.setValue (_inputElement_raw fileEl) ("" :: Text)
    pure metas
  countD <- foldDyn (+) 0 (length <$> metasE)
  let batchE = attachWith (\base ms -> zip [base ..] ms) (current countD) metasE
      retryJobsE = attachWithMaybe
        (\m i -> do
            e <- Map.lookup i m
            guard (isFailed (ueStatus e))
            pure [(i, ueMeta e, ueUploadId e)])
        (current entriesD) retryE
      jobsE = leftmost [map (\(i, meta) -> (i, meta, Nothing)) <$> batchE, retryJobsE]

  sem <- liftIO (newQSem 2)
  msgE <- performEventAsync $ ffor (attach (current (_inputElement_value nameEl)) jobsE) $ \(name, jobs) emit -> do
    ctx <- askJSM
    liftIO $ forM_ jobs $ \(i, meta, mUploadId) -> do
      emit (UpMsg i (SetStatus UpQueued))
      forkIO $ bracket_ (waitQSem sem) (signalQSem sem) $
        runJSM (uploadFile (nonBlank name) emit i meta mUploadId) ctx

  entriesD <- foldDyn ($) Map.empty $ leftmost
    [ (\batch m -> foldr (\(i, meta) -> Map.insert i (UpEntry meta UpQueued Nothing)) m batch) <$> batchE
    , (\(UpMsg i u) -> Map.adjust (applyUpdate u) i) <$> msgE
    ]

  activeRef <- liftIO (newIORef False)
  performEvent_ $ (\active -> liftIO (writeIORef activeRef active)) <$> updated (anyActive <$> entriesD)
  liftJSM (installUnloadGuard activeRef)

  retryE <- elDynAttr "div" (ffor entriesD $ \m -> "class" =: "fotos-queue" <> if Map.null m then "hidden" =: "" else mempty) $ do
    elAttr "p" ("class" =: "fotos-summary" <> "aria-live" =: "polite") $ dynText (summaryText <$> entriesD)
    elDynAttr "p" (ffor entriesD $ \m -> "class" =: "fotos-warn" <> if anyActive m then mempty else "hidden" =: "") $
      text "No cierres esta p\225gina hasta que termine. Mant\233n la pantalla encendida."
    clicksE <- elAttr "ul" ("class" =: "fotos-list") $
      listViewWithKey (Map.mapKeys Down <$> entriesD) (\_ entryD -> queueRow entryD)
    pure (fmapMaybe (fmap (getDown . fst) . Map.lookupMin) clicksE)

  pure (() <$ ffilter (\(UpMsg _ u) -> case u of SetStatus UpDone -> True; _ -> False) msgE)

queueRow :: MonadWidget t m => Dynamic t UpEntry -> m (Event t ())
queueRow entryD =
  elDynAttr "li" (ffor entryD $ \e -> "class" =: ("fotos-row " <> statusClass (ueStatus e))) $ do
    elDynAttr "span" (ffor entryD $ \e -> "class" =: ("fotos-row-icon" <> if fmIsVideo (ueMeta e) then " is-video" else "") <> "aria-hidden" =: "true") blank
    elAttr "div" ("class" =: "fotos-row-main") $ do
      elAttr "p" ("class" =: "fotos-row-name") $ dynText (fmName . ueMeta <$> entryD)
      elAttr "div" ("class" =: "fotos-row-track") $
        elDynAttr "div" (ffor entryD $ \e -> "class" =: "fotos-row-bar" <> "style" =: ("transform: scaleX(" <> progressFraction (ueStatus e) <> ")")) blank
    elAttr "p" ("class" =: "fotos-row-status") $ dynText (statusText . ueStatus <$> entryD)
    (b, _) <- elDynAttr' "button"
      (ffor entryD $ \e -> "class" =: "fotos-retry" <> "type" =: "button" <> if isFailed (ueStatus e) then mempty else "hidden" =: "")
      (text "Reintentar")
    pure (domEvent Click b)

applyUpdate :: UpUpdate -> UpEntry -> UpEntry
applyUpdate (SetStatus s)   e = e { ueStatus = s }
applyUpdate (SetUploadId u) e = e { ueUploadId = u }

isFailed :: UpStatus -> Bool
isFailed (UpFailed _) = True
isFailed _            = False

isActive :: UpStatus -> Bool
isActive s = case s of
  UpQueued    -> True
  UpSending _ -> True
  UpRetrying  -> True
  _           -> False

anyActive :: Map Int UpEntry -> Bool
anyActive = any (isActive . ueStatus) . Map.elems

statusClass :: UpStatus -> Text
statusClass s = case s of
  UpDone     -> "is-done"
  UpFailed _ -> "is-failed"
  UpRetrying -> "is-retrying"
  _          -> "is-active"

progressFraction :: UpStatus -> Text
progressFraction s = case s of
  UpSending p -> T.pack (show (fromIntegral p / 100 :: Double))
  UpDone      -> "1"
  UpRetrying  -> "1"
  UpFailed _  -> "1"
  UpQueued    -> "0"

statusText :: UpStatus -> Text
statusText s = case s of
  UpQueued    -> "En cola"
  UpSending p -> "Subiendo " <> T.pack (show p) <> "%"
  UpRetrying  -> "Reintentando\8230"
  UpDone      -> "\10003 Listo"
  UpFailed m  -> m

summaryText :: Map Int UpEntry -> Text
summaryText m =
  let total = Map.size m
      done = length (filter ((== UpDone) . ueStatus) (Map.elems m))
      failed = length (filter (isFailed . ueStatus) (Map.elems m))
      base = T.pack (show done) <> " de " <> T.pack (show total) <> (if total == 1 then " listo" else " listos")
   in if done == total && total > 0
        then "\161Gracias! " <> base <> ". Aparecer\225n en la galer\237a en unos momentos."
        else base <> (if failed > 0 then " \183 " <> T.pack (show failed) <> " con error" else "")

nonBlank :: Text -> Maybe Text
nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s

fileMeta :: DOM.File -> JSM FileMeta
fileMeta f = do
  name <- File.getName f
  ctype <- Blob.getType f
  size <- Blob.getSize f
  let lower = T.toLower name
      isVideo = "video/" `T.isPrefixOf` T.toLower ctype
        || any (`T.isSuffixOf` lower) [".mov", ".mp4", ".m4v", ".webm", ".3gp", ".mkv", ".avi"]
  pure (FileMeta f name ctype (fromIntegral size) isVideo)

-- ── Upload loop ───────────────────────────────────────────────────────────────

-- | Upload one file in chunks, resuming from whatever the server holds after
-- any failure. Transient failures (no connection, 5xx, 409) back off and
-- retry for about two minutes before asking the guest to tap "Reintentar".
uploadFile :: Maybe Text -> (UpMsg -> IO ()) -> Int -> FileMeta -> Maybe Text -> JSM ()
uploadFile uploaderName emit i meta mExisting = do
  say (SetStatus (UpSending 0))
  started <- case mExisting of
    Just uid -> pure (Right (uid, defaultChunkSize))
    Nothing  -> withRetries 0 startUpload
  case started of
    Left err -> say (SetStatus (UpFailed err))
    Right (uid, chunk) -> do
      say (SetUploadId (Just uid))
      offset <- if isJust mExisting then fromMaybe 0 <$> fetchReceived uid else pure 0
      sendFrom uid chunk offset 0
  where
    say = liftIO . emit . UpMsg i
    size = fmSize meta

    startUpload = do
      let body = TE.decodeUtf8 . BL.toStrict . encode $
            MediaUploadInit (fmName meta) (fmType meta) size uploaderName
      (st, resp) <- xhr "POST" "/api/media/uploads" [("Content-Type", "application/json")] (JsonBody body) Nothing
      pure $ case st of
        200 -> maybe (Fatal "Respuesta inesperada del servidor.")
                     (\s -> Ok (musId s, musChunkSize s)) (decodeText resp)
        _ | transient st -> Transient
          | otherwise    -> Fatal (serverMessage resp)

    withRetries :: Int -> JSM (Attempt a) -> JSM (Either Text a)
    withRetries attempt action = do
      r <- action
      case r of
        Ok a      -> pure (Right a)
        Fatal msg -> pure (Left msg)
        Transient
          | attempt >= maxAttempts -> pure (Left "Se perdi\243 la conexi\243n.")
          | otherwise -> do
              say (SetStatus UpRetrying)
              liftIO (threadDelay (backoffMicros attempt))
              withRetries (attempt + 1) action

    sendFrom uid chunk offset attempt
      | offset >= size = say (SetStatus UpDone)
      | otherwise = do
          let end = min size (offset + chunk)
          blob <- Blob.slice (fmFile meta) (Just offset) (Just end) (Nothing :: Maybe Text)
          lastPct <- liftIO (newIORef (-1))
          let report loaded = do
                let pct = fromIntegral (min 99 (((offset + loaded) * 100) `div` max 1 size)) :: Int
                prev <- liftIO (readIORef lastPct)
                when (pct /= prev) $ do
                  liftIO (writeIORef lastPct pct)
                  say (SetStatus (UpSending pct))
          (st, resp) <- xhr "PUT" ("/api/media/uploads/" <> uid <> "?offset=" <> T.pack (show offset))
            [("Content-Type", "application/octet-stream")] (BlobBody blob) (Just report)
          case (st, decodeText resp :: Maybe MediaUploadProgress) of
            (200, Just p)
              | mupComplete p -> say (SetStatus UpDone)
              | otherwise     -> sendFrom uid chunk (mupReceived p) 0
            _ | transient st && attempt < maxAttempts -> do
                  say (SetStatus UpRetrying)
                  liftIO (threadDelay (backoffMicros attempt))
                  held <- fetchReceived uid
                  sendFrom uid chunk (fromMaybe offset held) (attempt + 1)
              | transient st -> say (SetStatus (UpFailed "Se perdi\243 la conexi\243n."))
              | st == 404 || st == 410 -> do
                  -- The server no longer has this upload; a retry starts over.
                  say (SetUploadId Nothing)
                  say (SetStatus (UpFailed "La subida expir\243."))
              | otherwise -> say (SetStatus (UpFailed (serverMessage resp)))

    fetchReceived uid = do
      (st, resp) <- xhr "GET" ("/api/media/uploads/" <> uid) [] NoBody Nothing
      pure $ if st == 200 then mupReceived <$> decodeText resp else Nothing

data Attempt a = Ok a | Transient | Fatal Text

defaultChunkSize :: Int64
defaultChunkSize = 8 * 1024 * 1024

maxAttempts :: Int
maxAttempts = 8

-- | 1, 2, 4, 8, 15, 30, 30, 30 seconds.
backoffMicros :: Int -> Int
backoffMicros attempt = 1000000 * ([1, 2, 4, 8, 15] ++ repeat 30) !! attempt

-- | No response at all (status 0), conflicts, throttling and server/gateway
-- errors (including Cloudflare's 52x) are worth retrying.
transient :: Word -> Bool
transient st = st == 0 || st == 409 || st == 429 || st >= 500

serverMessage :: Text -> Text
serverMessage body =
  let msg = T.dropAround (== '"') (T.strip body)
   in if T.null msg || T.length msg > 160 then "No se pudo subir el archivo." else msg

data XhrBody = NoBody | JsonBody Text | BlobBody DOM.Blob

-- | One XMLHttpRequest, awaited by the calling (forked) thread: ghcjs-dom's
-- send* block until load/error/abort and throw on the latter two. The
-- progress listener is attached before sending so it never misses events.
-- A watchdog aborts only after 60 s without upload progress, so a slow but
-- moving connection is never cut off. (xhr.timeout is deliberately unused:
-- ghcjs-dom's send does not listen for the timeout event and would hang.)
xhr :: Text -> Text -> [(Text, Text)] -> XhrBody -> Maybe (Int64 -> JSM ()) -> JSM (Word, Text)
xhr method url headers body mProgress = do
  request <- Xhr.newXMLHttpRequest
  Xhr.openSimple request method url
  forM_ headers $ \(k, v) -> Xhr.setRequestHeader request k v
  idleTicks <- liftIO (newIORef (0 :: Int))
  releaseProgress <- case mProgress of
    Nothing -> pure (pure ())
    Just report -> do
      upload <- Xhr.getUpload request
      liftIO $ on upload XhrEv.progress $ do
        ev <- event
        loaded <- Progress.getLoaded ev
        liftIO (writeIORef idleTicks 0)
        liftIO (report (fromIntegral loaded))
  watchdog <- liftIO $ forkIO $ forever $ do
    threadDelay 5000000
    n <- atomicModifyIORef' idleTicks (\k -> (k + 1, k + 1))
    when (n >= 12) (Xhr.abort request)
  result <- liftIO . try $ case body of
    NoBody      -> Xhr.send request
    JsonBody t  -> Xhr.sendString request t
    BlobBody b  -> Xhr.sendBlob request b
  liftIO (killThread watchdog >> releaseProgress)
  case result of
    Left (_ :: SomeException) -> pure (0, "")
    Right () -> do
      st <- Xhr.getStatus request
      resp <- fromMaybe "" <$> Xhr.getResponseText request
      pure (st, resp)

-- ── Remembered name & leave-page guard ───────────────────────────────────────

uploaderNameKey :: Text
uploaderNameKey = "weddingUploaderName"

loadUploaderName :: JSM Text
loadUploaderName = do
  r <- liftIO . try $ do
    w <- DOM.currentWindowUnchecked
    s <- Window.getLocalStorage w
    Storage.getItem s uploaderNameKey
  pure $ case r of
    Right (Just v)            -> v
    Right Nothing             -> ""
    Left (_ :: SomeException) -> ""

saveUploaderName :: Text -> JSM ()
saveUploaderName name = do
  r <- liftIO . try $ do
    w <- DOM.currentWindowUnchecked
    s <- Window.getLocalStorage w
    Storage.setItem s uploaderNameKey (T.strip name)
  case r of
    Left (_ :: SomeException) -> pure ()
    Right ()                  -> pure ()

-- | Ask before leaving while uploads are still running (the browser shows its
-- own generic confirmation).
installUnloadGuard :: IORef Bool -> JSM ()
installUnloadGuard activeRef = do
  w <- DOM.currentWindowUnchecked
  _ <- liftIO $ on w WindowEvents.beforeUnload $ do
    active <- liftIO (readIORef activeRef)
    when active $ do
      EventM.preventDefault
      ev <- event
      BeforeUnload.setReturnValue ev ("Hay archivos subiendo" :: Text)
  pure ()

-- ── CSS ───────────────────────────────────────────────────────────────────────
-- Same visual language as the site: dark walnut, candle gold (#d4b483),
-- cream type, Courier Prime with Great Vibes accents. Motion animates only
-- transform and opacity.

fotosCSS :: Text
fotosCSS = T.unlines
  -- ── Gallery band ──────────────────────────────────────────────────────────
  [ ".galeria {"
  , "  min-height: auto;"
  , "  padding: clamp(3.6rem, 9svh, 6.2rem) 0 clamp(6rem, 12svh, 9rem);"
  , "  background:"
  , "    radial-gradient(ellipse 70% 42% at 50% 0%, rgba(212,180,131,.11), transparent 72%),"
  , "    radial-gradient(ellipse 120% 80% at 50% 100%, rgba(8,5,2,.55), transparent 60%),"
  , "    #1a120d;"
  , "  overflow: clip;"
  -- .section isolates its stacking context, which would trap the lightbox
  -- under the fixed nav; the band needs no isolation of its own.
  , "  isolation: auto;"
  , "}"
  , ".galeria::before { content: none; }"
  , ".galeria-head { display: grid; justify-items: center; gap: .5rem; padding: 0 1.4rem; text-align: center; }"
  , ".galeria-live { display: inline-flex; align-items: center; gap: .6rem; font-size: .66rem; letter-spacing: .34em; color: #d4b483; }"
  , ".galeria-dot { position: relative; width: .5rem; height: .5rem; border-radius: 50%; background: #d4b483; }"
  , ".galeria-dot::after { content: ''; position: absolute; inset: 0; border-radius: 50%; background: #d4b483; animation: livePulse 2.4s cubic-bezier(.19,1,.22,1) infinite; }"
  , "@keyframes livePulse { from { transform: scale(1); opacity: .75; } to { transform: scale(3.4); opacity: 0; } }"
  , ".galeria-title { font-family: 'Great Vibes', cursive; font-weight: 400; font-size: clamp(2.6rem, 8vw, 3.9rem); line-height: 1.08; color: #f8f1e4; letter-spacing: 0; }"
  , ".galeria-count { font-size: .76rem; letter-spacing: .16em; color: rgba(240,235,224,.58); }"
  , ".galeria-viewport { --row-h: clamp(220px, 46svh, 480px); --gutter: max(1.2rem, calc((100vw - 1240px) / 2)); position: relative; margin-top: clamp(1.6rem, 4svh, 2.6rem); }"
  , ".galeria-track {"
  , "  display: flex;"
  , "  gap: clamp(.6rem, 1.3vw, 1rem);"
  , "  overflow-x: auto;"
  , "  overflow-y: hidden;"
  , "  scroll-snap-type: x proximity;"
  , "  scroll-behavior: smooth;"
  , "  scroll-padding-inline: var(--gutter);"
  , "  overscroll-behavior-x: contain;"
  , "  padding: .4rem var(--gutter) 1.6rem;"
  , "  scrollbar-width: none;"
  , "  outline: none;"
  , "}"
  , ".galeria-track::-webkit-scrollbar { display: none; }"
  , ".galeria-track:focus-visible { box-shadow: inset 0 0 0 1px rgba(212,180,131,.55); border-radius: 8px; }"
  , ".galeria-tile {"
  , "  position: relative;"
  , "  flex: 0 0 auto;"
  , "  height: var(--row-h);"
  , "  max-width: 86vw;"
  , "  padding: 0;"
  , "  border: 0;"
  , "  border-radius: 6px;"
  , "  overflow: hidden;"
  , "  cursor: zoom-in;"
  , "  font: inherit;"
  , "  color: inherit;"
  , "  background: #2a1f17;"
  , "  scroll-snap-align: start;"
  , "  box-shadow: 0 1px 2px rgba(8,5,2,.42), 0 10px 26px rgba(8,5,2,.36), 0 0 0 1px rgba(212,180,131,.08);"
  , "  animation: tileArrive .9s cubic-bezier(.19,1,.22,1) both;"
  , "  transition: transform .5s cubic-bezier(.19,1,.22,1);"
  , "  -webkit-tap-highlight-color: transparent;"
  , "}"
  , ".galeria-tile img { display: block; width: 100%; height: 100%; object-fit: cover; transition: transform .9s cubic-bezier(.19,1,.22,1); user-select: none; }"
  , ".galeria-tile::after { content: ''; position: absolute; inset: 0; pointer-events: none; background: linear-gradient(to top, rgba(10,6,3,.62) 0%, rgba(10,6,3,0) 36%); }"
  , "@media (hover: hover) {"
  , "  .galeria-tile:hover { transform: translateY(-4px); }"
  , "  .galeria-tile:hover img { transform: scale(1.045); }"
  , "}"
  , ".galeria-tile:focus-visible { outline: 2px solid #d4b483; outline-offset: 3px; }"
  , ".galeria-tile:active { transform: scale(.985); transition-duration: .15s; }"
  , "@keyframes tileArrive { from { opacity: 0; transform: translateX(-22px) scale(.95); } to { opacity: 1; transform: none; } }"
  , ".galeria-credit { position: absolute; left: .8rem; right: .8rem; bottom: .7rem; z-index: 1; font-size: .7rem; letter-spacing: .08em; color: rgba(255,250,240,.92); text-align: left; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }"
  , ".galeria-play { position: absolute; inset: 0; margin: auto; z-index: 1; width: 3.3rem; height: 3.3rem; border-radius: 50%; background: rgba(20,13,7,.42); border: 1px solid rgba(255,255,255,.6); backdrop-filter: blur(6px); -webkit-backdrop-filter: blur(6px); transition: transform .5s cubic-bezier(.19,1,.22,1); }"
  , ".galeria-play::before { content: ''; position: absolute; top: 50%; left: 54%; transform: translate(-50%, -50%); border-style: solid; border-width: .52rem 0 .52rem .85rem; border-color: transparent transparent transparent #fff; }"
  , "@media (hover: hover) { .galeria-tile:hover .galeria-play { transform: scale(1.08); } }"
  , ".galeria-duration { position: absolute; top: .6rem; right: .6rem; z-index: 1; padding: .16rem .5rem; border-radius: 999px; background: rgba(20,13,7,.62); color: #fff; font-size: .66rem; letter-spacing: .06em; }"
  , ".galeria-empty { position: relative; flex: 0 0 auto; display: flex; gap: inherit; width: min(calc(100vw - 2.4rem), 1240px); overflow: hidden; border-radius: 8px; }"
  , ".galeria-empty[hidden] { display: none; }"
  , ".galeria-ghost { position: relative; flex: 0 0 auto; height: var(--row-h); aspect-ratio: 3 / 4; border-radius: 6px; overflow: hidden; background: rgba(138,108,76,.08); border: 1px dashed rgba(212,180,131,.22); }"
  , ".galeria-ghost:nth-child(2) { aspect-ratio: 4 / 3; }"
  , ".galeria-ghost::after { content: ''; position: absolute; inset: 0; background: linear-gradient(110deg, transparent 30%, rgba(212,180,131,.13) 50%, transparent 70%); transform: translateX(-100%); animation: ghostShimmer 2.8s ease-in-out infinite; }"
  , ".galeria-ghost:nth-child(2)::after { animation-delay: .25s; }"
  , ".galeria-ghost:nth-child(3)::after { animation-delay: .5s; }"
  , "@keyframes ghostShimmer { to { transform: translateX(100%); } }"
  , ".galeria-empty-copy { position: absolute; inset: 0; display: grid; place-items: center; padding: 0 2rem; text-align: center; font-size: .92rem; line-height: 1.8; letter-spacing: .04em; color: rgba(240,235,224,.82); background: radial-gradient(ellipse 60% 50% at 50% 50%, rgba(26,18,13,.78), rgba(26,18,13,.2) 80%); }"
  , "@media (max-width: 760px) { .galeria-viewport { --row-h: clamp(220px, 38svh, 340px); } .galeria-tile { max-width: 90vw; } }"
  , ".galeria-nav { display: none; }"
  , "@media (hover: hover) and (min-width: 761px) {"
  , "  .galeria-nav {"
  , "    display: grid;"
  , "    place-items: center;"
  , "    position: absolute;"
  , "    top: calc(.4rem + var(--row-h) / 2);"
  , "    z-index: 2;"
  , "    width: 3rem;"
  , "    height: 3rem;"
  , "    border-radius: 50%;"
  , "    border: 1px solid rgba(255,255,255,.26);"
  , "    background: rgba(28,20,16,.66);"
  , "    backdrop-filter: blur(10px);"
  , "    -webkit-backdrop-filter: blur(10px);"
  , "    color: #f0ebe0;"
  , "    font: inherit;"
  , "    font-size: 1.1rem;"
  , "    cursor: pointer;"
  , "    transform: translateY(-50%);"
  , "    box-shadow: 0 8px 24px rgba(0,0,0,.32);"
  , "    transition: transform .35s cubic-bezier(.19,1,.22,1);"
  , "  }"
  , "  .galeria-prev { left: max(.7rem, calc(var(--gutter) - 1.5rem)); }"
  , "  .galeria-next { right: max(.7rem, calc(var(--gutter) - 1.5rem)); }"
  , "  .galeria-nav:hover { transform: translateY(-50%) scale(1.08); border-color: rgba(212,180,131,.7); }"
  , "  .galeria-nav:active { transform: translateY(-50%) scale(.96); }"
  , "  .galeria-nav:focus-visible { outline: 2px solid #d4b483; outline-offset: 2px; }"
  , "}"

  -- ── Lightbox ──────────────────────────────────────────────────────────────
  , "body:has(.lightbox) { overflow: hidden; }"
  , ".lightbox { position: fixed; inset: 0; z-index: 700; }"
  , ".lightbox-backdrop { position: absolute; inset: 0; background: rgba(12,8,5,.93); backdrop-filter: blur(8px); -webkit-backdrop-filter: blur(8px); animation: lbFade .3s ease both; }"
  , ".lightbox-frame { position: relative; z-index: 1; width: 100%; height: 100%; display: grid; place-items: center; padding: 4.2rem clamp(.5rem, 5vw, 5.5rem); outline: none; pointer-events: none; }"
  , ".lightbox-frame > * { pointer-events: auto; }"
  , ".lightbox-media { display: grid; place-items: center; max-width: 100%; max-height: 100%; animation: lbIn .5s cubic-bezier(.19,1,.22,1) both; }"
  , ".lightbox-img, .lightbox-video { display: block; max-width: min(100%, 1700px); max-height: calc(100svh - 8.4rem); width: auto; height: auto; border-radius: 4px; background: #000; box-shadow: 0 30px 80px rgba(0,0,0,.55), 0 0 0 1px rgba(212,180,131,.12); }"
  , ".lightbox-credit { position: absolute; left: 4rem; right: 4rem; bottom: 1.35rem; text-align: center; font-size: .78rem; letter-spacing: .12em; color: rgba(240,235,224,.78); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }"
  , ".lightbox-btn { position: absolute; display: grid; place-items: center; width: 2.9rem; height: 2.9rem; border-radius: 50%; border: 1px solid rgba(255,255,255,.26); background: rgba(28,20,16,.62); color: #f0ebe0; font: inherit; font-size: 1.15rem; cursor: pointer; transition: transform .3s cubic-bezier(.19,1,.22,1), border-color .3s; }"
  , ".lightbox-btn:hover { border-color: rgba(212,180,131,.75); }"
  , ".lightbox-btn:focus-visible { outline: 2px solid #d4b483; outline-offset: 2px; }"
  , ".lightbox-btn:active { transform: scale(.94); }"
  , ".lightbox-close { top: .9rem; right: .9rem; font-size: 1.55rem; }"
  , ".lightbox-prev { left: .9rem; top: 50%; margin-top: -1.45rem; }"
  , ".lightbox-next { right: .9rem; top: 50%; margin-top: -1.45rem; }"
  , "@media (max-width: 760px) {"
  , "  .lightbox-frame { padding: 3.8rem .5rem 4.6rem; }"
  , "  .lightbox-prev, .lightbox-next { top: auto; bottom: .8rem; margin-top: 0; }"
  , "  .lightbox-credit { bottom: 1.6rem; }"
  , "}"
  , "@keyframes lbFade { from { opacity: 0; } to { opacity: 1; } }"
  , "@keyframes lbIn { from { opacity: 0; transform: translateY(14px) scale(.97); } to { opacity: 1; transform: none; } }"

  -- ── Upload page (/fotos) ──────────────────────────────────────────────────
  , ".fotos-page {"
  , "  min-height: 100svh;"
  , "  background:"
  , "    radial-gradient(ellipse 80% 38% at 50% 0%, rgba(212,180,131,.16), transparent 70%),"
  , "    radial-gradient(ellipse 50% 30% at 50% 36%, rgba(255,214,150,.05), transparent 70%),"
  , "    #1c1410;"
  , "}"
  , ".fotos-header { position: relative; display: grid; justify-items: center; padding: 3.6rem 1.4rem 1.8rem; text-align: center; }"
  , ".fotos-back { position: absolute; top: .9rem; left: .9rem; padding: .45rem .3rem; font-size: .7rem; letter-spacing: .16em; text-transform: uppercase; color: rgba(240,235,224,.66); text-decoration: none; transition: color .25s; }"
  , ".fotos-back:hover { color: #d4b483; }"
  , ".fotos-back:focus-visible { outline: 1px solid #d4b483; outline-offset: 2px; }"
  , ".fotos-kicker { font-size: .68rem; letter-spacing: .32em; color: #d4b483; animation: fotosRise .8s cubic-bezier(.19,1,.22,1) both; }"
  , ".fotos-names { margin-top: .6rem; font-family: 'Great Vibes', cursive; font-weight: 400; font-size: clamp(2.9rem, 12.5vw, 4.8rem); line-height: 1.04; color: #fbf6ec; animation: fotosRise .9s cubic-bezier(.19,1,.22,1) .08s both; }"
  , ".fotos-date { margin-top: .45rem; font-size: .8rem; letter-spacing: .34em; color: rgba(240,235,224,.66); animation: fotosRise .9s cubic-bezier(.19,1,.22,1) .16s both; }"
  , "@keyframes fotosRise { from { opacity: 0; transform: translateY(14px); } to { opacity: 1; transform: none; } }"
  , ".fotos-card {"
  , "  display: grid;"
  , "  gap: 1rem;"
  , "  width: min(calc(100vw - 2rem), 30rem);"
  , "  margin: 0 auto;"
  , "  padding: 1.6rem 1.3rem 1.4rem;"
  , "  border-radius: 18px;"
  , "  background: linear-gradient(180deg, rgba(138,108,76,.20), rgba(138,108,76,.10));"
  , "  border: 1px solid rgba(255,255,255,.14);"
  , "  box-shadow: 0 1px 1px rgba(8,5,2,.3), 0 14px 36px rgba(8,5,2,.36), 0 40px 90px rgba(8,5,2,.3);"
  , "  text-align: center;"
  , "  animation: fotosRise 1s cubic-bezier(.19,1,.22,1) .22s both;"
  , "}"
  , ".fotos-copy { font-size: .92rem; line-height: 1.7; color: rgba(240,235,224,.9); }"
  , ".fotos-name { margin-bottom: 0; text-align: center; font-size: .92rem; }"
  , ".fotos-pick {"
  , "  position: relative;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  gap: .75rem;"
  , "  min-height: 3.7rem;"
  , "  padding: .9rem 1.2rem;"
  , "  border-radius: 12px;"
  , "  background: linear-gradient(180deg, #e6cb9d 0%, #c9a46d 100%);"
  , "  color: #23160c;"
  , "  font-size: 1.02rem;"
  , "  letter-spacing: .07em;"
  , "  cursor: pointer;"
  , "  box-shadow: inset 0 1px 0 rgba(255,255,255,.4), 0 2px 6px rgba(0,0,0,.3), 0 12px 30px rgba(212,180,131,.22);"
  , "  transition: transform .35s cubic-bezier(.19,1,.22,1);"
  , "  -webkit-tap-highlight-color: transparent;"
  , "}"
  , ".fotos-pick:hover { transform: translateY(-2px); }"
  , ".fotos-pick:active { transform: scale(.985); transition-duration: .12s; }"
  , ".fotos-pick:focus-within { outline: 2px solid #f0ebe0; outline-offset: 3px; }"
  , ".fotos-file { position: absolute; width: 1px; height: 1px; opacity: 0; overflow: hidden; }"
  , ".fotos-pick-icon { position: relative; width: 1.45rem; height: 1.45rem; border-radius: 50%; border: 1.5px solid currentColor; }"
  , ".fotos-pick-icon::before, .fotos-pick-icon::after { content: ''; position: absolute; top: 50%; left: 50%; width: .7rem; height: 1.5px; background: currentColor; transform: translate(-50%, -50%); }"
  , ".fotos-pick-icon::after { transform: translate(-50%, -50%) rotate(90deg); }"
  , ".fotos-hint { font-size: .76rem; line-height: 1.6; color: rgba(240,235,224,.58); }"
  , ".fotos-queue { display: grid; gap: .6rem; text-align: left; }"
  , ".fotos-queue[hidden], .fotos-warn[hidden], .fotos-retry[hidden] { display: none; }"
  , ".fotos-summary { font-size: .84rem; line-height: 1.6; letter-spacing: .04em; color: #f0ebe0; text-align: center; }"
  , ".fotos-warn { font-size: .74rem; line-height: 1.55; color: #ffdfb4; text-align: center; }"
  , ".fotos-list { list-style: none; display: grid; gap: .5rem; max-height: 44svh; overflow-y: auto; overscroll-behavior: contain; }"
  , ".fotos-row { display: grid; grid-template-columns: auto 1fr auto; align-items: center; gap: .3rem .75rem; padding: .6rem .7rem; border-radius: 10px; background: rgba(20,13,7,.38); border: 1px solid rgba(255,255,255,.07); animation: fotosRise .5s cubic-bezier(.19,1,.22,1) both; }"
  , ".fotos-row-icon { position: relative; width: 1.9rem; height: 1.9rem; border-radius: 7px; background: rgba(212,180,131,.16); }"
  , ".fotos-row-icon::before { content: ''; position: absolute; inset: .5rem .42rem; border: 1.5px solid #d4b483; border-radius: 3px; }"
  , ".fotos-row-icon.is-video::before { inset: auto; top: 50%; left: 54%; border-radius: 0; border-style: solid; border-width: .4rem 0 .4rem .65rem; border-color: transparent transparent transparent #d4b483; transform: translate(-50%, -50%); }"
  , ".fotos-row-main { min-width: 0; }"
  , ".fotos-row-name { font-size: .78rem; color: rgba(240,235,224,.9); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }"
  , ".fotos-row-track { margin-top: .38rem; height: 3px; border-radius: 3px; background: rgba(255,255,255,.12); overflow: hidden; }"
  , ".fotos-row-bar { height: 100%; background: linear-gradient(90deg, #c9a46d, #efd7a8); transform-origin: left center; transform: scaleX(0); transition: transform .35s ease; }"
  , ".fotos-row.is-done .fotos-row-bar { background: #a9d1a3; }"
  , ".fotos-row.is-failed .fotos-row-bar { background: #ffb4a8; }"
  , ".fotos-row.is-retrying .fotos-row-bar { opacity: .45; }"
  , ".fotos-row-status { font-size: .72rem; color: rgba(240,235,224,.68); white-space: nowrap; }"
  , ".fotos-row.is-done .fotos-row-status { color: #bfe0b9; }"
  , ".fotos-row.is-failed .fotos-row-status { color: #ffb4a8; white-space: normal; text-align: right; }"
  , ".fotos-retry { grid-column: 2 / -1; justify-self: end; padding: .32rem .85rem; border-radius: 999px; border: 1px solid rgba(255,180,168,.55); background: rgba(255,180,168,.1); color: #ffd6ce; font: inherit; font-size: .72rem; letter-spacing: .08em; cursor: pointer; transition: transform .25s cubic-bezier(.19,1,.22,1); }"
  , ".fotos-retry:hover { transform: translateY(-1px); }"
  , ".fotos-retry:active { transform: scale(.96); }"
  , ".fotos-retry:focus-visible { outline: 2px solid #ffb4a8; outline-offset: 2px; }"
  , ".fotos-page .galeria { padding-top: clamp(2.8rem, 7svh, 4.4rem); padding-bottom: clamp(3rem, 8svh, 5rem); background: transparent; }"

  -- ── Main-site invitation card for the fotos section ───────────────────────
  , ".fotos-invite-card { text-align: center; margin: 1.1rem auto; transform: translateY(var(--card-lift)); }"
  , ".fotos-invite-copy { font-size: .78em; line-height: 1.65; color: rgba(255,255,255,.88); }"
  , ".fotos-invite-card .qr-block { margin-top: .9rem; }"
  , ".fotos-invite-card .registry-link-btn { white-space: nowrap; letter-spacing: .04em; }"

  , "@media (prefers-reduced-motion: reduce) {"
  , "  .galeria-dot::after, .galeria-ghost::after { animation: none; }"
  , "  .galeria-tile, .lightbox-media, .lightbox-backdrop, .fotos-row, .fotos-card, .fotos-kicker, .fotos-names, .fotos-date { animation: none; }"
  , "  .galeria-track { scroll-behavior: auto; }"
  , "}"
  ]
