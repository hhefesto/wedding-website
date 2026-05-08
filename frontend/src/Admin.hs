{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson               (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy     as BL
import           Control.Monad            (void)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Language.Javascript.JSaddle (MonadJSM, eval, liftJSM)
import           Reflex.Dom
import           Text.Read                (readMaybe)
import           Wedding.Types            (AttendanceStatus (..), Invitee (..),
                                            IpAssociationAdmin (..), IpAssociationInput (..),
                                            InviteeInput (..), LinkInviteeBody (..),
                                            LoginRequest (..), RsvpAdmin (..),
                                            ResolveDuplicateBody (..),
                                            VideoAdmin (..))

main :: IO ()
main = mainWidgetWithHead headW adminRoot

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Wedding dashboard"
  el "style" $ text adminCSS

data AdminTab = TabInvitees | TabRsvps | TabVideos | TabIps
  deriving (Eq)

data RsvpAdminAction = LinkRsvp Text (Maybe Int64) | DeleteRsvp Text | ResolveDuplicate Text Text
data VideoAdminAction = LinkVideo Text (Maybe Int64) | DeleteVideo Text
data IpAdminAction = CreateIp IpAssociationInput | UpdateIp Int64 IpAssociationInput | DeleteIp Int64

adminRoot :: (MonadWidget t m, MonadJSM (Performable m)) => m ()
adminRoot = mdo
  pb <- getPostBuild
  performEvent_ $ liftJSM (void $ eval adminCopyQrJS) <$ pb
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
  ipsRespE <- performRequestAsync (xhrGet "/api/admin/ip-associations" <$ loadE)
  inviteesDyn <- holdDyn [] (decodeXhrList <$> inviteesRespE)
  rsvpsDyn <- holdDyn [] (decodeXhrList <$> rsvpsRespE)
  videosDyn <- holdDyn [] (decodeXhrList <$> videosRespE)
  ipsDyn <- holdDyn [] (decodeXhrList <$> ipsRespE)
  refreshE <- elAttr "section" ("class" =: "admin-panel") $ do
    panelDyn <- dyn $ ffor tabDyn $ \tab -> case tab of
      TabInvitees -> adminInviteesPanel inviteesDyn
      TabRsvps    -> adminRsvpsPanel inviteesDyn rsvpsDyn
      TabVideos   -> adminVideosPanel inviteesDyn videosDyn
      TabIps      -> adminIpsPanel inviteesDyn ipsDyn
    switchHold never panelDyn
  logoutRespE <- performRequestAsync (xhrPostNoBody "/api/admin/logout" <$ logoutClickE)
  pure (xhrOk logoutRespE)

adminTabs :: MonadWidget t m => m (Dynamic t AdminTab)
adminTabs = elAttr "nav" ("class" =: "admin-tabs") $ mdo
  (inviteBtn, _) <- elDynAttr' "button" (tabAttrs TabInvitees <$> tabDyn) $ text "Invitados"
  (rsvpBtn, _) <- elDynAttr' "button" (tabAttrs TabRsvps <$> tabDyn) $ text "RSVPs"
  (videoBtn, _) <- elDynAttr' "button" (tabAttrs TabVideos <$> tabDyn) $ text "Videos"
  (ipBtn, _) <- elDynAttr' "button" (tabAttrs TabIps <$> tabDyn) $ text "IPs"
  tabDyn <- holdDyn TabInvitees $ leftmost
    [ TabInvitees <$ domEvent Click inviteBtn
    , TabRsvps    <$ domEvent Click rsvpBtn
    , TabVideos   <$ domEvent Click videoBtn
    , TabIps      <$ domEvent Click ipBtn
    ]
  pure tabDyn

adminInviteesPanel :: MonadWidget t m => Dynamic t [Invitee] -> m (Event t ())
adminInviteesPanel inviteesDyn = elAttr "div" ("class" =: "admin-grid") $ mdo
  createOkE <- elAttr "div" ("class" =: "admin-card admin-form") $ do
    el "h2" $ text "Agregar invitado"
    nameEl <- adminInput "text" "Nombre" ""
    codeEl <- adminInput "text" "Codigo de invitacion" ""
    maxEl <- adminInput "number" "Max adultos (max 2)" "1"
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
    dyn_ $ ffor inviteeDyn $ \invitee -> case inviteeCode invitee of
      Nothing -> blank
      Just code -> el "p" $
        elAttr "a" ("class" =: "admin-link small" <> "href" =: inviteeUrl code <> "target" =: "_blank" <> "rel" =: "noopener noreferrer") $
          text "Abrir liga RSVP"
  dyn_ $ ffor inviteeDyn $ \invitee -> case inviteeCode invitee of
    Nothing -> blank
    Just code -> elAttr "div" ("class" =: "admin-qr-box") $ do
      elAttr "img" (inviteeQrAttrs invitee) blank
      elAttr "button" (copyQrAttrs invitee code) $ text "Copiar QR"
      elAttr "button" (copyLinkAttrs code) $ text "Copiar liga"
  (btnEl, _) <- elAttr' "button" ("class" =: "admin-danger" <> "type" =: "button") $ text "Eliminar"
  pure (fromIntegral . inviteeId <$> current inviteeDyn `tag` domEvent Click btnEl)

adminRsvpsPanel :: MonadWidget t m => Dynamic t [Invitee] -> Dynamic t [RsvpAdmin] -> m (Event t ())
adminRsvpsPanel inviteesDyn rsvpsDyn = elAttr "div" ("class" =: "admin-card") $ do
  el "h2" $ do
    text "RSVPs ("
    dynText (T.pack . show . length <$> rsvpsDyn)
    text ") - "
    dynText (T.pack . show . totalGuests <$> rsvpsDyn)
    text " adultos"
  actionDyn <- elAttr "div" ("class" =: "admin-list") $ simpleList rsvpsDyn (adminRsvpRow inviteesDyn rsvpsDyn)
  let actionE = switchDyn (leftmost <$> actionDyn)
      reqE = ffor actionE $ \action -> case action of
        LinkRsvp rid miid -> adminJsonRequest "PUT" ("/api/admin/rsvps/" <> rid <> "/invitee") (LinkInviteeBody miid)
        DeleteRsvp rid    -> xhrDelete ("/api/admin/rsvps/" <> rid)
        ResolveDuplicate rid keep -> adminJsonRequest "POST" ("/api/admin/rsvps/" <> rid <> "/resolve-duplicate") (ResolveDuplicateBody keep)
  respE <- performRequestAsync reqE
  pure (xhrOk respE)

adminRsvpRow :: MonadWidget t m => Dynamic t [Invitee] -> Dynamic t [RsvpAdmin] -> Dynamic t RsvpAdmin -> m (Event t RsvpAdminAction)
adminRsvpRow inviteesDyn rsvpsDyn rsvpDyn = elAttr "article" ("class" =: "admin-row") $ do
  el "div" $ do
    el "strong" $ dynText (raName <$> rsvpDyn)
    el "p" $ dynText (rsvpMeta <$> rsvpDyn)
    el "p" $ dynText (rsvpInviteeMeta <$> rsvpDyn)
    el "p" $ dynText (rsvpResolutionMeta <$> rsvpDyn)
    el "p" $ dynText (maybe "" id . raDietary <$> rsvpDyn)
  elAttr "div" ("class" =: "admin-row-actions") $ do
    pb <- getPostBuild
    let selectedInviteeD = maybe "" (T.pack . show) . raInviteeId <$> rsvpDyn
    selectedDyn <- dropdown "" (inviteeDropdownOptions <$> inviteesDyn <*> rsvpsDyn <*> rsvpDyn) $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "admin-input")
      & dropdownConfig_setValue .~ leftmost [current selectedInviteeD `tag` pb, updated selectedInviteeD]
    (linkBtn, _) <- elAttr' "button" ("class" =: "admin-btn small" <> "type" =: "button") $ text "Ligar"
    (unlinkBtn, _) <- elAttr' "button" ("class" =: "admin-btn small ghost" <> "type" =: "button") $ text "Sin invitacion"
    (keepExistingBtn, _) <- elAttr' "button" ("class" =: "admin-btn small ghost" <> "type" =: "button") $ text "Mantener RSVP existente"
    (keepNewBtn, _) <- elAttr' "button" ("class" =: "admin-btn small" <> "type" =: "button") $ text "Mantener este RSVP"
    (deleteBtn, _) <- elAttr' "button" ("class" =: "admin-danger" <> "type" =: "button") $ text "Eliminar RSVP"
    let ridD = raId <$> rsvpDyn
        parsedIdD = parseMaybeInt64 <$> _dropdown_value selectedDyn
    pure $ leftmost
      [ attachWith LinkRsvp (current ridD) (current parsedIdD `tag` domEvent Click linkBtn)
      , attachWith (\rid _ -> LinkRsvp rid Nothing) (current ridD) (domEvent Click unlinkBtn)
      , attachWith (\rid _ -> ResolveDuplicate rid "existing") (current ridD) (domEvent Click keepExistingBtn)
      , attachWith (\rid _ -> ResolveDuplicate rid "new") (current ridD) (domEvent Click keepNewBtn)
      , DeleteRsvp <$> (current ridD `tag` domEvent Click deleteBtn)
      ]

adminVideosPanel :: MonadWidget t m => Dynamic t [Invitee] -> Dynamic t [VideoAdmin] -> m (Event t ())
adminVideosPanel inviteesDyn videosDyn = elAttr "div" ("class" =: "admin-card") $ do
  el "h2" $ do
    text "Videos ("
    dynText (T.pack . show . length <$> videosDyn)
    text ")"
  actionDyn <- elAttr "div" ("class" =: "admin-list") $ simpleList videosDyn (adminVideoRow inviteesDyn)
  let actionE = switchDyn (leftmost <$> actionDyn)
      reqE = ffor actionE $ \action -> case action of
        LinkVideo vid miid -> adminJsonRequest "PUT" ("/api/admin/videos/" <> vid <> "/invitee") (LinkInviteeBody miid)
        DeleteVideo vid    -> xhrDelete ("/api/admin/videos/" <> vid)
  respE <- performRequestAsync reqE
  pure (xhrOk respE)

adminVideoRow :: MonadWidget t m => Dynamic t [Invitee] -> Dynamic t VideoAdmin -> m (Event t VideoAdminAction)
adminVideoRow inviteesDyn videoDyn = elAttr "article" ("class" =: "admin-row") $ do
  el "div" $ do
    el "strong" $ dynText (vaOriginalFilename <$> videoDyn)
    el "p" $ dynText (videoMeta <$> videoDyn)
    el "p" $ dynText (videoSubmitterMeta <$> videoDyn)
  elAttr "div" ("class" =: "admin-row-actions") $ do
    pb <- getPostBuild
    let selectedInviteeD = maybe "" (T.pack . show) . vaInviteeId <$> videoDyn
    selectedDyn <- dropdown "" (simpleInviteeDropdownOptions <$> inviteesDyn) $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "admin-input")
      & dropdownConfig_setValue .~ leftmost [current selectedInviteeD `tag` pb, updated selectedInviteeD]
    elDynAttr "a" (videoDownloadAttrs <$> videoDyn) $ text "Descargar"
    (linkBtn, _) <- elAttr' "button" ("class" =: "admin-btn small" <> "type" =: "button") $ text "Ligar"
    (unlinkBtn, _) <- elAttr' "button" ("class" =: "admin-btn small ghost" <> "type" =: "button") $ text "Sin invitacion"
    (deleteBtn, _) <- elAttr' "button" ("class" =: "admin-danger" <> "type" =: "button") $ text "Eliminar"
    let vidD = vaId <$> videoDyn
        parsedIdD = parseMaybeInt64 <$> _dropdown_value selectedDyn
    pure $ leftmost
      [ attachWith LinkVideo (current vidD) (current parsedIdD `tag` domEvent Click linkBtn)
      , attachWith (\vid _ -> LinkVideo vid Nothing) (current vidD) (domEvent Click unlinkBtn)
      , DeleteVideo <$> (current vidD `tag` domEvent Click deleteBtn)
      ]

adminIpsPanel :: MonadWidget t m => Dynamic t [Invitee] -> Dynamic t [IpAssociationAdmin] -> m (Event t ())
adminIpsPanel inviteesDyn ipsDyn = elAttr "div" ("class" =: "admin-card") $ do
  el "h2" $ do
    text "IPs ("
    dynText (T.pack . show . length <$> ipsDyn)
    text ")"
  elAttr "p" ("class" =: "admin-muted") $ text "Relaciona direcciones IP conocidas con invitaciones para resolver RSVP y videos sin codigo."
  createE <- adminIpCreateForm inviteesDyn
  actionDyn <- elAttr "div" ("class" =: "admin-list") $ simpleList ipsDyn (adminIpRow inviteesDyn)
  let actionE = leftmost [createE, switchDyn (leftmost <$> actionDyn)]
      reqE = ffor actionE $ \action -> case action of
        CreateIp input -> adminJsonRequest "POST" "/api/admin/ip-associations" input
        UpdateIp aid input -> adminJsonRequest "PUT" ("/api/admin/ip-associations/" <> T.pack (show aid)) input
        DeleteIp aid -> xhrDelete ("/api/admin/ip-associations/" <> T.pack (show aid))
  respE <- performRequestAsync reqE
  pure (xhrOk respE)

adminIpCreateForm :: MonadWidget t m => Dynamic t [Invitee] -> m (Event t IpAdminAction)
adminIpCreateForm inviteesDyn = elAttr "div" ("class" =: "admin-inline-form") $ do
  selectedDyn <- dropdown "" (simpleInviteeDropdownOptions <$> inviteesDyn) $ def
    & dropdownConfig_attributes .~ constDyn ("class" =: "admin-input")
  ipEl <- adminInput "text" "IP (ej. 203.0.113.10)" ""
  sourceEl <- adminInput "text" "Fuente" "admin"
  (btn, _) <- elAttr' "button" ("class" =: "admin-btn small" <> "type" =: "button") $ text "Agregar IP"
  let inputDyn = IpAssociationInput
        <$> (maybe 0 id . parseMaybeInt64 <$> _dropdown_value selectedDyn)
        <*> (T.strip <$> _inputElement_value ipEl)
        <*> (T.strip <$> _inputElement_value sourceEl)
  pure (CreateIp <$> current inputDyn `tag` domEvent Click btn)

adminIpRow :: MonadWidget t m => Dynamic t [Invitee] -> Dynamic t IpAssociationAdmin -> m (Event t IpAdminAction)
adminIpRow inviteesDyn ipDyn = elAttr "article" ("class" =: "admin-row") $ do
  el "div" $ do
    el "strong" $ dynText (ipaIpAddress <$> ipDyn)
    el "p" $ dynText (ipAssociationMeta <$> ipDyn)
  elAttr "div" ("class" =: "admin-row-actions") $ do
    pb <- getPostBuild
    let selectedInviteeD = T.pack . show . ipaInviteeId <$> ipDyn
    selectedDyn <- dropdown "" (simpleInviteeDropdownOptions <$> inviteesDyn) $ def
      & dropdownConfig_attributes .~ constDyn ("class" =: "admin-input")
      & dropdownConfig_setValue .~ leftmost [current selectedInviteeD `tag` pb, updated selectedInviteeD]
    ipEl <- inputElement $ def
      & inputElementConfig_setValue .~ leftmost [current (ipaIpAddress <$> ipDyn) `tag` pb, updated (ipaIpAddress <$> ipDyn)]
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "admin-input" <> "type" =: "text")
    sourceEl <- inputElement $ def
      & inputElementConfig_setValue .~ leftmost [current (ipaSource <$> ipDyn) `tag` pb, updated (ipaSource <$> ipDyn)]
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "admin-input" <> "type" =: "text")
    (saveBtn, _) <- elAttr' "button" ("class" =: "admin-btn small" <> "type" =: "button") $ text "Guardar"
    (deleteBtn, _) <- elAttr' "button" ("class" =: "admin-danger" <> "type" =: "button") $ text "Eliminar"
    let aidD = ipaId <$> ipDyn
        inputDyn = IpAssociationInput
          <$> (maybe 0 id . parseMaybeInt64 <$> _dropdown_value selectedDyn)
          <*> (T.strip <$> _inputElement_value ipEl)
          <*> (T.strip <$> _inputElement_value sourceEl)
    pure $ leftmost
      [ attachWith UpdateIp (current aidD) (current inputDyn `tag` domEvent Click saveBtn)
      , DeleteIp <$> (current aidD `tag` domEvent Click deleteBtn)
      ]

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
parseCount value = maybe 1 (max 1 . min 2) (readMaybe (T.unpack value))

parseMaybeInt64 :: Text -> Maybe Int64
parseMaybeInt64 value = readMaybe (T.unpack (T.strip value))

emptyToMaybe :: Text -> Maybe Text
emptyToMaybe value = let stripped = T.strip value in if T.null stripped then Nothing else Just stripped

inviteeMeta :: Invitee -> Text
inviteeMeta i = "ID " <> T.pack (show (inviteeId i)) <> " - " <> maybe "sin codigo" id (inviteeCode i) <> " - max " <> T.pack (show (inviteeMaxGuests i)) <> " adultos"

inviteeUrl :: Text -> Text
inviteeUrl code = "/?code=" <> code

inviteeQrAttrs :: Invitee -> Map Text Text
inviteeQrAttrs invitee =
  "class" =: "admin-qr"
  <> "src" =: ("/api/admin/invitees/" <> T.pack (show (inviteeId invitee)) <> "/qr")
  <> "alt" =: ("QR RSVP " <> inviteeName invitee)

copyQrAttrs :: Invitee -> Text -> Map Text Text
copyQrAttrs invitee code =
  "class" =: "admin-btn small admin-copy-qr"
  <> "type" =: "button"
  <> "data-qr-src" =: ("/api/admin/invitees/" <> T.pack (show (inviteeId invitee)) <> "/qr")
  <> "data-rsvp-link" =: inviteeUrl code

copyLinkAttrs :: Text -> Map Text Text
copyLinkAttrs code =
  "class" =: "admin-btn small ghost admin-copy-link"
  <> "type" =: "button"
  <> "data-rsvp-link" =: inviteeUrl code

statusLabel :: AttendanceStatus -> Text
statusLabel Attending = "attending"
statusLabel Declined  = "declined"

rsvpMeta :: RsvpAdmin -> Text
rsvpMeta r = statusLabel (raStatus r) <> " - " <> T.pack (show (raGuestCount r)) <> " adultos - " <> raCreatedAt r

rsvpInviteeMeta :: RsvpAdmin -> Text
rsvpInviteeMeta r =
  case raInviteeId r of
    Nothing -> "Sin invitacion asociada"
    Just iid -> "Invitacion: " <> maybe ("ID " <> T.pack (show iid)) id (raInviteeName r) <> " - Codigo: " <> maybe "none" id (raInviteeCode r)

rsvpResolutionMeta :: RsvpAdmin -> Text
rsvpResolutionMeta r = T.intercalate " | " (filter (not . T.null)
  [ "IP: " <> maybe "sin IP" id (raIpAddress r)
  , "Estado: " <> raResolutionStatus r
  , if raResolutionStatus r == "review"
      then "Revisar contra: " <> maybe "sin sugerencia" id (raSuggestedName r) <> maybe "" (" / RSVP " <>) (raSuggestedRsvpId r)
      else ""
  ])

inviteeDropdownOptions :: [Invitee] -> [RsvpAdmin] -> RsvpAdmin -> Map Text Text
inviteeDropdownOptions invitees rsvps rsvp = Map.fromList $
  ("", "Selecciona invitacion libre") : map optionFor (filter available invitees)
  where
    currentId = raInviteeId rsvp
    available invitee = Just (inviteeId invitee) == currentId || inviteeId invitee `notElem` assignedIds
    assignedIds = [iid | other <- rsvps, Just iid <- [raInviteeId other], Just iid /= currentId]
    optionFor invitee = (T.pack (show (inviteeId invitee)), inviteeName invitee <> " - " <> maybe "sin codigo" id (inviteeCode invitee))

simpleInviteeDropdownOptions :: [Invitee] -> Map Text Text
simpleInviteeDropdownOptions invitees = Map.fromList $
  ("", "Selecciona invitacion") : map optionFor invitees
  where
    optionFor invitee = (T.pack (show (inviteeId invitee)), inviteeName invitee <> " - " <> maybe "sin codigo" id (inviteeCode invitee))

totalGuests :: [RsvpAdmin] -> Int
totalGuests = sum . map (\r -> if raStatus r == Attending then raGuestCount r else 0)

videoMeta :: VideoAdmin -> Text
videoMeta v = vaContentType v <> " - " <> T.pack (show (fromIntegral (vaSizeBytes v) / (1048576 :: Double))) <> " MB - " <> vaCreatedAt v

videoSubmitterMeta :: VideoAdmin -> Text
videoSubmitterMeta v = T.intercalate " | " (filter (not . T.null)
  [ "Subido por: " <> maybe "anonimo" id (vaSubmitterName v)
  , "RSVP: " <> maybe "sin RSVP" id (vaRsvpName v)
  , "Invitacion: " <> maybe "sin invitacion" id (vaInviteeName v)
  , "IP: " <> maybe "sin IP" id (vaIpAddress v)
  , "Estado: " <> vaResolutionStatus v
  , maybe "" ("Mensaje: " <>) (vaMessage v)
  ])

ipAssociationMeta :: IpAssociationAdmin -> Text
ipAssociationMeta a = T.intercalate " | "
  [ "Invitacion: " <> ipaInviteeName a <> " - Codigo: " <> maybe "none" id (ipaInviteeCode a)
  , "Fuente: " <> ipaSource a
  , "Primera vez: " <> ipaFirstSeenAt a
  , "Ultima vez: " <> ipaLastSeenAt a
  ]

videoDownloadAttrs :: VideoAdmin -> Map Text Text
videoDownloadAttrs v =
  "class" =: "admin-btn small" <> "href" =: ("/api/admin/videos/" <> vaId v <> "/download")

adminCopyQrJS :: Text
adminCopyQrJS = T.concat
  [ "(function(){"
  , "if(window.__weddingAdminCopyQrReady)return;window.__weddingAdminCopyQrReady=1;"
  , "function abs(u){return new URL(u,window.location.origin).href;}"
  , "function note(m){var n=document.getElementById('admin-copy-status');if(!n){n=document.createElement('div');n.id='admin-copy-status';n.className='admin-copy-status';document.body.appendChild(n);}n.textContent=m;setTimeout(function(){n.textContent='';},3500);}"
  , "function copyText(t){return navigator.clipboard.writeText(t).then(function(){note('Liga copiada.');});}"
  , "document.addEventListener('click',function(e){var qr=e.target.closest&&e.target.closest('.admin-copy-qr');var link=e.target.closest&&e.target.closest('.admin-copy-link');if(!qr&&!link)return;"
  , "if(link){copyText(abs(link.dataset.rsvpLink||'/')).catch(function(){note('No se pudo copiar la liga.');});return;}"
  , "var fallback=abs(qr.dataset.rsvpLink||'/');var src=qr.dataset.qrSrc;if(!navigator.clipboard||!window.ClipboardItem){copyText(fallback).catch(function(){window.open(src,'_blank');});return;}"
  , "fetch(src,{credentials:'same-origin'}).then(function(r){if(!r.ok)throw new Error(String(r.status));return r.blob();}).then(function(b){return navigator.clipboard.write([new ClipboardItem({'image/png':b})]);}).then(function(){note('QR copiado. Pegalo en WhatsApp.');}).catch(function(){copyText(fallback).catch(function(){window.open(src,'_blank');});});"
  , "});"
  , "})()"
  ]

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
  , "button.admin-btn { appearance: none; }"
  , ".admin-link.small { padding: .4rem .68rem; font-size: .72rem; margin-top: .25rem; }"
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
  , ".admin-row-actions { display: flex; flex-wrap: wrap; gap: .45rem; align-items: center; justify-content: flex-end; max-width: 420px; }"
  , ".admin-inline-form { display: grid; grid-template-columns: minmax(180px, 1.2fr) minmax(160px, 1fr) minmax(120px, .7fr) auto; gap: .55rem; align-items: center; margin-bottom: .8rem; }"
  , ".admin-muted.tiny { width: 100%; font-size: .7rem; max-height: 4.2rem; overflow: auto; }"
  , ".admin-qr-box { display: grid; gap: .45rem; justify-items: center; }"
  , ".admin-qr { width: 96px; height: 96px; padding: .35rem; background: rgba(255,255,255,.94); border-radius: 10px; object-fit: contain; }"
  , ".admin-copy-status { position: fixed; right: 1rem; bottom: 1rem; z-index: 50; min-height: 1.5rem; color: #160f0a; background: #d4b483; border-radius: 999px; padding: .55rem .85rem; box-shadow: 0 14px 40px rgba(0,0,0,.32); }"
  , ".admin-danger { border: 1px solid rgba(255,120,105,.45); color: #ffd7d1; background: rgba(255,120,105,.10); border-radius: 999px; padding: .46rem .72rem; cursor: pointer; }"
  , ".admin-error { color: #ffb4a8; min-height: 1.2rem; margin-top: .8rem; }"
  , "@media (max-width: 760px) { .admin-top { align-items: flex-start; flex-direction: column; } .admin-actions { justify-content: flex-start; } .admin-grid { grid-template-columns: 1fr; } .admin-row { align-items: flex-start; flex-direction: column; } .admin-inline-form { grid-template-columns: 1fr; } }"
  ]
