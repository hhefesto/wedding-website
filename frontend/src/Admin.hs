{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson               (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy     as BL
import           Data.Map                 (Map)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Reflex.Dom
import           Text.Read                (readMaybe)
import           Wedding.Types            (AttendanceStatus (..), Invitee (..),
                                           InviteeInput (..), LoginRequest (..),
                                           RsvpAdmin (..), VideoAdmin (..))

main :: IO ()
main = mainWidgetWithHead headW adminRoot

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Wedding dashboard"
  el "style" $ text adminCSS

data AdminTab = TabInvitees | TabRsvps | TabVideos
  deriving (Eq)

adminRoot :: MonadWidget t m => m ()
adminRoot = mdo
  pb <- getPostBuild
  meRespE <- performRequestAsync (xhrGet "/api/admin/me" <$ pb)
  authDyn <- holdDyn False $ leftmost
    [ True <$ xhrOk meRespE
    , True <$ loginOkE
    , False <$ logoutDoneE
    ]
  loginOkE <- elDynAttr "div" (visibleAttrs . not <$> authDyn) adminLogin
  logoutDoneE <- elDynAttr "div" (visibleAttrs <$> authDyn) (adminDashboard loginOkE)
  pure ()

adminLogin :: MonadWidget t m => m (Event t ())
adminLogin = elAttr "main" ("class" =: "admin-page") $
  elAttr "section" ("class" =: "admin-login") $ mdo
    elAttr "p" ("class" =: "admin-kicker") $ text "ADMIN"
    el "h1" $ text "Wedding dashboard"
    elAttr "p" ("class" =: "admin-muted") $ text "Administra invitados, RSVP y videos."
    passEl <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ( "class" =: "admin-input" <> "type" =: "password" <> "placeholder" =: "Password" <> "autocomplete" =: "current-password" )
    (btnEl, _) <- elAttr' "button" ("class" =: "admin-btn" <> "type" =: "button") $ text "Entrar"
    let loginReqDyn = ffor (_inputElement_value passEl) $ \password ->
          adminJsonRequest "POST" "/api/admin/login" (LoginRequest password)
    respE <- performRequestAsync (current loginReqDyn `tag` domEvent Click btnEl)
    let okE = xhrOk respE
        badE = ffilter not (xhrSuccess <$> respE)
    msgDyn <- holdDyn "" $ leftmost ["" <$ okE, "Password inv\225lido." <$ badE]
    elAttr "p" ("class" =: "admin-error") $ dynText msgDyn
    pure okE

adminDashboard :: MonadWidget t m => Event t () -> m (Event t ())
adminDashboard loggedInE = elAttr "main" ("class" =: "admin-page") $ mdo
  pb <- getPostBuild
  logoutClickE <- elAttr "header" ("class" =: "admin-top") $ do
    el "div" $ do
      elAttr "p" ("class" =: "admin-kicker") $ text "ADMIN"
      el "h1" $ text "Wedding dashboard"
    elAttr "div" ("class" =: "admin-actions") $ do
      elAttr "a" ("class" =: "admin-link" <> "href" =: "/") $ text "Sitio publico"
      (logoutBtn, _) <- elAttr' "button" ("class" =: "admin-btn ghost" <> "type" =: "button") $ text "Salir"
      pure (domEvent Click logoutBtn)
  tabDyn <- adminTabs
  let loadE = leftmost [() <$ pb, loggedInE, refreshE]
  inviteesRespE <- performRequestAsync (xhrGet "/api/admin/invitees" <$ loadE)
  rsvpsRespE <- performRequestAsync (xhrGet "/api/admin/rsvps" <$ loadE)
  videosRespE <- performRequestAsync (xhrGet "/api/admin/videos" <$ loadE)
  inviteesDyn <- holdDyn [] (decodeXhrList <$> inviteesRespE)
  rsvpsDyn <- holdDyn [] (decodeXhrList <$> rsvpsRespE)
  videosDyn <- holdDyn [] (decodeXhrList <$> videosRespE)
  refreshE <- elAttr "section" ("class" =: "admin-panel") $ do
    panelDyn <- dyn $ ffor tabDyn $ \tab -> case tab of
      TabInvitees -> adminInviteesPanel inviteesDyn
      TabRsvps    -> adminRsvpsPanel rsvpsDyn
      TabVideos   -> adminVideosPanel videosDyn
    switchHold never panelDyn
  logoutRespE <- performRequestAsync (xhrPostNoBody "/api/admin/logout" <$ logoutClickE)
  pure (xhrOk logoutRespE)

adminTabs :: MonadWidget t m => m (Dynamic t AdminTab)
adminTabs = elAttr "nav" ("class" =: "admin-tabs") $ mdo
  (inviteBtn, _) <- elDynAttr' "button" (tabAttrs TabInvitees <$> tabDyn) $ text "Invitados"
  (rsvpBtn, _) <- elDynAttr' "button" (tabAttrs TabRsvps <$> tabDyn) $ text "RSVPs"
  (videoBtn, _) <- elDynAttr' "button" (tabAttrs TabVideos <$> tabDyn) $ text "Videos"
  tabDyn <- holdDyn TabInvitees $ leftmost
    [ TabInvitees <$ domEvent Click inviteBtn
    , TabRsvps    <$ domEvent Click rsvpBtn
    , TabVideos   <$ domEvent Click videoBtn
    ]
  pure tabDyn

adminInviteesPanel :: MonadWidget t m => Dynamic t [Invitee] -> m (Event t ())
adminInviteesPanel inviteesDyn = elAttr "div" ("class" =: "admin-grid") $ mdo
  createOkE <- elAttr "div" ("class" =: "admin-card admin-form") $ do
    el "h2" $ text "Agregar invitado"
    nameEl <- adminInput "text" "Nombre" ""
    codeEl <- adminInput "text" "Codigo de invitacion" ""
    maxEl <- adminInput "number" "Max asistentes" "1"
    notesEl <- adminTextArea "Notas"
    (btnEl, _) <- elAttr' "button" ("class" =: "admin-btn" <> "type" =: "button") $ text "Agregar"
    let inputDyn = InviteeInput
          <$> _inputElement_value nameEl
          <*> (emptyToMaybe <$> _inputElement_value codeEl)
          <*> (parseCount <$> _inputElement_value maxEl)
          <*> (emptyToMaybe <$> _textAreaElement_value notesEl)
        reqDyn = adminJsonRequest "POST" "/api/admin/invitees" <$> inputDyn
    respE <- performRequestAsync (current reqDyn `tag` domEvent Click btnEl)
    pure (xhrOk respE)
  deleteE <- elAttr "div" ("class" =: "admin-card") $ do
    el "h2" $ do
      text "Invitados ("
      dynText (T.pack . show . length <$> inviteesDyn)
      text ")"
    deleteDyn <- elAttr "div" ("class" =: "admin-list") $ simpleList inviteesDyn adminInviteeRow
    pure (switchDyn (leftmost <$> deleteDyn))
  deleteRespE <- performRequestAsync (xhrDelete . ("/api/admin/invitees/" <>) . T.pack . show <$> deleteE)
  pure $ leftmost [createOkE, xhrOk deleteRespE]

adminInviteeRow :: MonadWidget t m => Dynamic t Invitee -> m (Event t Int)
adminInviteeRow inviteeDyn = elAttr "article" ("class" =: "admin-row") $ do
  el "div" $ do
    el "strong" $ dynText (inviteeName <$> inviteeDyn)
    el "p" $ dynText (inviteeMeta <$> inviteeDyn)
    el "p" $ dynText (maybe "" id . inviteeNotes <$> inviteeDyn)
  (btnEl, _) <- elAttr' "button" ("class" =: "admin-danger" <> "type" =: "button") $ text "Eliminar"
  pure (fromIntegral . inviteeId <$> current inviteeDyn `tag` domEvent Click btnEl)

adminRsvpsPanel :: MonadWidget t m => Dynamic t [RsvpAdmin] -> m (Event t ())
adminRsvpsPanel rsvpsDyn = elAttr "div" ("class" =: "admin-card") $ do
  el "h2" $ do
    text "RSVPs ("
    dynText (T.pack . show . length <$> rsvpsDyn)
    text ") - "
    dynText (T.pack . show . totalGuests <$> rsvpsDyn)
    text " asistentes"
  elAttr "div" ("class" =: "admin-list") $ simpleList rsvpsDyn adminRsvpRow
  pure never

adminRsvpRow :: MonadWidget t m => Dynamic t RsvpAdmin -> m ()
adminRsvpRow rsvpDyn = elAttr "article" ("class" =: "admin-row") $ el "div" $ do
  el "strong" $ dynText (raName <$> rsvpDyn)
  el "p" $ dynText (rsvpMeta <$> rsvpDyn)
  el "p" $ dynText (rsvpInviteeMeta <$> rsvpDyn)
  el "p" $ dynText (maybe "" id . raDietary <$> rsvpDyn)

adminVideosPanel :: MonadWidget t m => Dynamic t [VideoAdmin] -> m (Event t ())
adminVideosPanel videosDyn = elAttr "div" ("class" =: "admin-card") $ do
  el "h2" $ do
    text "Videos ("
    dynText (T.pack . show . length <$> videosDyn)
    text ")"
  elAttr "div" ("class" =: "admin-list") $ simpleList videosDyn adminVideoRow
  pure never

adminVideoRow :: MonadWidget t m => Dynamic t VideoAdmin -> m ()
adminVideoRow videoDyn = elAttr "article" ("class" =: "admin-row") $ do
  el "div" $ do
    el "strong" $ dynText (vaOriginalFilename <$> videoDyn)
    el "p" $ dynText (videoMeta <$> videoDyn)
    el "p" $ dynText (videoSubmitterMeta <$> videoDyn)
  elDynAttr "a" (videoDownloadAttrs <$> videoDyn) $ text "Descargar"

adminInput :: MonadWidget t m => Text -> Text -> Text -> m (InputElement EventResult (DomBuilderSpace m) t)
adminInput inputType placeholder value = inputElement $ def
  & inputElementConfig_initialValue .~ value
  & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
    ("class" =: "admin-input" <> "type" =: inputType <> "placeholder" =: placeholder)

adminTextArea :: MonadWidget t m => Text -> m (TextAreaElement EventResult (DomBuilderSpace m) t)
adminTextArea placeholder = textAreaElement $ def
  & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
    ("class" =: "admin-input" <> "placeholder" =: placeholder)

xhrGet :: Text -> XhrRequest Text
xhrGet url = XhrRequest "GET" url $ def & xhrRequestConfig_sendData .~ ""

xhrDelete :: Text -> XhrRequest Text
xhrDelete url = XhrRequest "DELETE" url $ def & xhrRequestConfig_sendData .~ ""

xhrPostNoBody :: Text -> XhrRequest Text
xhrPostNoBody url = XhrRequest "POST" url $ def & xhrRequestConfig_sendData .~ ""

adminJsonRequest :: ToJSON a => Text -> Text -> a -> XhrRequest Text
adminJsonRequest method url value = XhrRequest method url $ def
  & xhrRequestConfig_headers .~ ("Content-Type" =: "application/json")
  & xhrRequestConfig_sendData .~ TE.decodeUtf8 (BL.toStrict (encode value))

xhrSuccess :: XhrResponse -> Bool
xhrSuccess resp = let s = _xhrResponse_status resp in s >= 200 && s < 300

xhrOk :: Reflex t => Event t XhrResponse -> Event t ()
xhrOk = (() <$) . ffilter xhrSuccess

decodeXhrList :: FromJSON a => XhrResponse -> [a]
decodeXhrList resp = case decode (BL.fromStrict (TE.encodeUtf8 (maybe "[]" id (_xhrResponse_responseText resp)))) of
  Just xs -> xs
  Nothing -> []

visibleAttrs :: Bool -> Map Text Text
visibleAttrs True  = mempty
visibleAttrs False = "style" =: "display:none"

tabAttrs :: AdminTab -> AdminTab -> Map Text Text
tabAttrs mine current =
  "type" =: "button" <> "class" =: if mine == current then "active" else ""

parseCount :: Text -> Int
parseCount value = maybe 1 (max 1 . min 20) (readMaybe (T.unpack value))

emptyToMaybe :: Text -> Maybe Text
emptyToMaybe value = let stripped = T.strip value in if T.null stripped then Nothing else Just stripped

inviteeMeta :: Invitee -> Text
inviteeMeta i = maybe "sin codigo" id (inviteeCode i) <> " - max " <> T.pack (show (inviteeMaxGuests i)) <> " asistentes"

statusLabel :: AttendanceStatus -> Text
statusLabel Attending = "attending"
statusLabel Declined  = "declined"

rsvpMeta :: RsvpAdmin -> Text
rsvpMeta r = statusLabel (raStatus r) <> " - " <> T.pack (show (raGuestCount r)) <> " asistentes - " <> raCreatedAt r

rsvpInviteeMeta :: RsvpAdmin -> Text
rsvpInviteeMeta r =
  "Invitado: " <> maybe "sin ligar" (T.pack . show) (raInviteeId r) <> " - Codigo: " <> maybe "none" id (raInvitationCodeUsed r)

totalGuests :: [RsvpAdmin] -> Int
totalGuests = sum . map (\r -> if raStatus r == Attending then raGuestCount r else 0)

videoMeta :: VideoAdmin -> Text
videoMeta v = vaContentType v <> " - " <> T.pack (show (fromIntegral (vaSizeBytes v) / (1048576 :: Double))) <> " MB - " <> vaCreatedAt v

videoSubmitterMeta :: VideoAdmin -> Text
videoSubmitterMeta v = maybe "anonimo" id (vaSubmitterName v) <> " " <> maybe "" id (vaMessage v)

videoDownloadAttrs :: VideoAdmin -> Map Text Text
videoDownloadAttrs v =
  "class" =: "admin-btn small" <> "href" =: ("/api/admin/videos/" <> vaId v <> "/download")

adminCSS :: Text
adminCSS = T.unlines
  [ "@import url('https://fonts.googleapis.com/css2?family=Courier+Prime:ital,wght@0,400;1,400&display=swap');"
  , "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'Courier Prime', 'Courier New', monospace; background: #160f0a; color: #f0ebe0; overflow-x: hidden; }"
  , ".admin-page { min-height: 100svh; padding: clamp(1rem, 4vw, 3rem); color: #f0ebe0; background: radial-gradient(circle at 20% 0%, rgba(176,129,76,.18), transparent 36%), #160f0a; }"
  , ".admin-top { display: flex; justify-content: space-between; gap: 1rem; align-items: center; max-width: 1160px; margin: 0 auto 1.2rem; }"
  , ".admin-top h1, .admin-login h1 { font-weight: 400; letter-spacing: .04em; }"
  , ".admin-kicker { color: #d4b483; letter-spacing: .28em; font-size: .68rem; margin-bottom: .35rem; }"
  , ".admin-muted { color: rgba(255,255,255,.68); line-height: 1.7; margin: .8rem 0 1.1rem; }"
  , ".admin-login { width: min(92vw, 420px); margin: 12vh auto 0; padding: 2rem; border: 1px solid rgba(255,255,255,.16); border-radius: 22px; background: rgba(138,108,76,.18); box-shadow: 0 24px 70px rgba(0,0,0,.38); }"
  , ".admin-input { width: 100%; margin: .45rem 0; padding: .78rem .88rem; border-radius: 10px; border: 1px solid rgba(255,255,255,.22); background: rgba(255,255,255,.08); color: #f0ebe0; font-family: 'Courier Prime', monospace; }"
  , ".admin-input:focus { outline: none; border-color: rgba(212,180,131,.75); }"
  , ".admin-btn, .admin-link { display: inline-flex; align-items: center; justify-content: center; gap: .4rem; border: 1px solid rgba(212,180,131,.55); color: #fff; background: rgba(212,180,131,.12); border-radius: 999px; padding: .62rem 1rem; text-decoration: none; cursor: pointer; font-family: 'Courier Prime', monospace; font-size: .86rem; }"
  , ".admin-btn:hover, .admin-link:hover { background: rgba(212,180,131,.22); }"
  , ".admin-btn.ghost { background: transparent; border-color: rgba(255,255,255,.26); }"
  , ".admin-btn.small { padding: .48rem .8rem; font-size: .78rem; }"
  , ".admin-actions { display: flex; flex-wrap: wrap; gap: .6rem; justify-content: flex-end; }"
  , ".admin-tabs { max-width: 1160px; margin: 0 auto 1.2rem; display: flex; gap: .55rem; flex-wrap: wrap; }"
  , ".admin-tabs button { border: 1px solid rgba(255,255,255,.16); background: rgba(255,255,255,.05); color: rgba(255,255,255,.72); border-radius: 999px; padding: .55rem .9rem; cursor: pointer; font-family: 'Courier Prime', monospace; }"
  , ".admin-tabs button.active { color: #160f0a; background: #d4b483; border-color: #d4b483; }"
  , ".admin-panel { max-width: 1160px; margin: 0 auto; }"
  , ".admin-grid { display: grid; grid-template-columns: minmax(260px, 360px) 1fr; gap: 1rem; align-items: start; }"
  , ".admin-card { border: 1px solid rgba(255,255,255,.14); border-radius: 20px; background: rgba(255,255,255,.06); padding: 1rem; box-shadow: 0 18px 50px rgba(0,0,0,.24); }"
  , ".admin-card h2 { font-weight: 400; font-size: 1rem; letter-spacing: .08em; margin-bottom: .8rem; color: #fff; }"
  , ".admin-list { display: grid; gap: .7rem; }"
  , ".admin-row { display: flex; justify-content: space-between; gap: .8rem; align-items: center; padding: .8rem; border: 1px solid rgba(255,255,255,.10); border-radius: 14px; background: rgba(0,0,0,.14); }"
  , ".admin-row strong { color: #fff; font-weight: 400; }"
  , ".admin-row p { margin-top: .25rem; color: rgba(255,255,255,.65); font-size: .82rem; line-height: 1.45; }"
  , ".admin-danger { border: 1px solid rgba(255,120,105,.45); color: #ffd7d1; background: rgba(255,120,105,.10); border-radius: 999px; padding: .46rem .72rem; cursor: pointer; }"
  , ".admin-error { color: #ffb4a8; min-height: 1.2rem; margin-top: .8rem; }"
  , "@media (max-width: 760px) { .admin-top { align-items: flex-start; flex-direction: column; } .admin-actions { justify-content: flex-start; } .admin-grid { grid-template-columns: 1fr; } .admin-row { align-items: flex-start; flex-direction: column; } }"
  ]
