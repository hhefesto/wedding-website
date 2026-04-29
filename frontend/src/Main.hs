{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import Control.Monad (forM_, void)
import Language.Javascript.JSaddle (eval, MonadJSM, liftJSM)
import Reflex.Dom
import Wedding.Types (Rsvp (..))

-- ── Entry point ───────────────────────────────────────────────────────────────

main :: IO ()
main = mainWidgetWithHead headW bodyW

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Daniel y Ana Cristina — 10 · 10 · 26"
  el "style" $ text siteCSS

-- ── Body ──────────────────────────────────────────────────────────────────────

bodyW :: (MonadWidget t m, MonadJSM (Performable m)) => m ()
bodyW = do
  videoOpenE <- elAttr "div" ("class" =: "site-shell") $ do
    introOverlay
    progressBar
    heroSection
    rsvpOpenE <- rsvpSection
    ubicacionSection
    dressCodeSection
    mesaRegalosSection
    videoOpenE' <- videoMsgSection
    fixedNav
    backToTop
    rsvpOverlay rsvpOpenE
    pure videoOpenE'
  adminRoot
  videoUploadOverlay videoOpenE
  pb <- getPostBuild
  performEvent_ $ liftJSM (void $ eval navHighlightingJS) <$ pb

-- ── Intro overlay ─────────────────────────────────────────────────────────────
-- Full-screen panel that plays the invitation text then fades out.

introOverlay :: DomBuilder t m => m ()
introOverlay =
  elAttr "div" ("id" =: "intro" <> "class" =: "intro") $
    elAttr "div" ("class" =: "intro-inner") $ do
      elAttr "p" ("class" =: "intro-kicker") $
        staggerWords ["Te", "invitamos", "a", "nuestra", "boda"]
      elAttr "span" ("class" =: "intro-rule") blank
      elAttr "p" ("class" =: "intro-sign") $
        text "atte. Cristy y Daniel"

-- Wrap each word in a span with a --i custom property for CSS stagger.
staggerWords :: DomBuilder t m => [Text] -> m ()
staggerWords ws =
  forM_ (zip [(0 :: Int) ..] ws) $ \(i, w) -> do
    elAttr "span"
      ( "class" =: "intro-word"
     <> "style" =: ("--i:" <> T.pack (show i))
      ) $ text w
    text "\xa0"

-- ── Progress bar ──────────────────────────────────────────────────────────────

progressBar :: DomBuilder t m => m ()
progressBar =
  elAttr "div" ("id" =: "progress-bar" <> "class" =: "progress-bar") blank

-- ── Back to top ──────────────────────────────────────────────────────────────

backToTop :: DomBuilder t m => m ()
backToTop =
  elAttr "a"
    ( "id"         =: "back-to-top"
   <> "class"      =: "back-to-top"
   <> "href"       =: "#hero"
   <> "aria-label" =: "Volver arriba"
    ) blank

-- ── HERO ─────────────────────────────────────────────────────────────────────

heroSection :: DomBuilder t m => m ()
heroSection =
  sec "hero" $ do
    elAttr "div" ("class" =: "hero-bg") blank
    elAttr "div" ("class" =: "hero-spacer") blank
    elAttr "div" ("class" =: "hero-copy") $
      elAttr "p" ("class" =: "hero-date") $ text "10/10/26"

-- ── Fixed bottom navigation ───────────────────────────────────────────────────
-- Persistent glassmorphism bar. Slides in after intro via CSS animation.
-- Active link highlighting is driven by IntersectionObserver (navHighlightingJS).

navHighlightingJS :: String
navHighlightingJS =
  "(function(){"
  <> "var obs=new IntersectionObserver(function(entries){"
  <> "entries.forEach(function(e){"
  <> "var id=e.target.id;"
  <> "var lnk=document.querySelector('[data-section=\"'+id+'\"]');"
  <> "if(lnk){lnk.classList.toggle('is-active',e.isIntersecting);}"
  <> "});"
  <> "},{rootMargin:'-40% 0px -40% 0px',threshold:0});"
  <> "document.querySelectorAll('.image-section').forEach(function(s){obs.observe(s);});"
  <> "})()"

fixedNav :: DomBuilder t m => m ()
fixedNav =
  elAttr "nav"
    ( "id"         =: "fixed-nav"
   <> "class"      =: "fixed-nav"
   <> "aria-label" =: "Secciones"
    ) $
    forM_ navItems $ \(href, label) ->
      elAttr "a"
        ( "href"         =: href
       <> "class"        =: "fixed-nav-link"
       <> "data-section" =: T.drop 1 href
        ) $ text label
  where
    navItems :: [(Text, Text)]
    navItems =
      [ ("#rsvp",          "RSVP")
      , ("#ubicacion",     "UBICACI\211N")
      , ("#dress-code",    "DRESS CODE")
      , ("#mesa-regalos",  "REGALOS")
      , ("#video-mensaje", "VIDEO")
      ]

-- ── UBICACIÓN ────────────────────────────────────────────────────────────────

ubicacionSection :: DomBuilder t m => m ()
ubicacionSection =
  secImage "ubicacion" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "images/2.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "UBICACI\211N"
      elAttr "div" ("class" =: "glass rect ubicacion-card" <> "data-reveal" =: "") $ do
        el "p" $ text "Gran Terraza"
        el "p" $ text "Vista Real Country Club"
        el "p" $ text "6 pm"
        elAttr "iframe"
          ( "class"          =: "map-embed"
         <> "src"            =: "https://maps.google.com/maps?q=20.5229282,-100.4039031&z=17&output=embed&hl=es"
         <> "allowfullscreen" =: ""
         <> "loading"        =: "lazy"
         <> "referrerpolicy" =: "no-referrer-when-downgrade"
          ) blank

-- ── DRESS CODE ───────────────────────────────────────────────────────────────

dressCodeSection :: DomBuilder t m => m ()
dressCodeSection =
  secImage "dress-code" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "images/3.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    -- label + glass card anchored to the top of the section
    elAttr "div" ("class" =: "section-overlay dress-code-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "DRESS CODE"
      elAttr "div" ("class" =: "glass rect dress-info" <> "data-reveal" =: "") $ do
        el "p" $ text "Formal"
        el "p" $ text "H: traje y corbata"
        el "p" $ text "M: corto, midi, largo"

-- ── RSVP ─────────────────────────────────────────────────────────────────────

rsvpSection :: DomBuilder t m => m (Event t ())
rsvpSection =
  secImage "rsvp" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "./images/6.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $ text "RSVP"
      e <- elAttr "div" ("class" =: "glass rect rsvp-confirm" <> "data-reveal" =: "") $ do
        el "p" $ text "Por favor confirma tu asistencia"
        el "p" $ text "antes del 10 de septiembre de 2026."
        (btnEl, _) <- elAttr' "button" ("class" =: "rsvp-btn") $ text "Confirmar \8594"
        return (() <$ domEvent Click btnEl)
      return e

-- ── RSVP overlay — pure Reflex state machine ─────────────────────────────────
-- All 4 step divs stay in the DOM; stepDyn drives CSS show/hide so inputs
-- retain their values while hidden. WhatsApp href is built in pure Haskell.

rsvpOverlay :: MonadWidget t m => Event t () -> m ()
rsvpOverlay openE = mdo
  visibleDyn <- holdDyn False $ leftmost [True <$ openE, False <$ closeE]
  stepDyn <- foldDyn ($) (1 :: Int) $ leftmost
    [ const 1         <$ openE
    , min 4 . (+1) <$ nextE
    ]

  let overlayAttrs = ffor visibleDyn $ \v ->
        "id" =: "rsvp-overlay" <> "class" =: "rsvp-overlay"
          <> if v then mempty else "style" =: "display:none"

  (closeE, nextE) <- elDynAttr "div" overlayAttrs $ do
    (closeBtnEl, _) <- elAttr' "button" ("class" =: "rsvp-close") $ text "\215"

    (n1E, n2E, n3E, nameD, guestD, dietaryD) <-
      elAttr "div" ("class" =: "rsvp-modal") $ do

        -- Step 1: name
        (nameD', n1E') <- rsvpStep stepDyn 1 $ do
          elAttr "p" ("class" =: "rsvp-step-label") $ text "\191C\243mo te llamas?"
          ti <- inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              (  "type"        =: "text"
              <> "class"       =: "rsvp-input"
              <> "placeholder" =: "Tu nombre completo"
              )
          (nb, _) <- elAttr' "button" ("class" =: "rsvp-btn") $ text "Continuar \8594"
          return (_inputElement_value ti, domEvent Click nb)

        -- Step 2: guest count (mdo for counter buttons)
        (guestD', n2E') <- rsvpStep stepDyn 2 $ mdo
          elAttr "p" ("class" =: "rsvp-step-label") $ text "\191Cu\225ntos asistir\225n?"
          countDyn <- foldDyn ($) (1 :: Int) $ leftmost
            [ (\n -> max 1  (n - 1)) <$ minusE
            , (\n -> min 10 (n + 1)) <$ plusE
            , const 1               <$ openE
            ]
          (minusE, plusE) <- elAttr "div" ("class" =: "rsvp-counter") $ do
            (minEl, _) <- elAttr' "button" ("class" =: "rsvp-counter-btn") $ text "\8722"
            el "span" $ dynText (T.pack . show <$> countDyn)
            (plusEl, _) <- elAttr' "button" ("class" =: "rsvp-counter-btn") $ text "+"
            return (domEvent Click minEl, domEvent Click plusEl)
          (nb, _) <- elAttr' "button" ("class" =: "rsvp-btn") $ text "Continuar \8594"
          return (countDyn, domEvent Click nb)

        -- Step 3: dietary restrictions
        (dietaryD', n3E') <- rsvpStep stepDyn 3 $ do
          elAttr "p" ("class" =: "rsvp-step-label") $
            text "\191Alguna restricci\243n alimentaria?"
          ti <- inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              (  "type"        =: "text"
              <> "class"       =: "rsvp-input"
              <> "placeholder" =: "Opcional"
              )
          (nb, _) <- elAttr' "button" ("class" =: "rsvp-btn") $ text "Continuar \8594"
          return (_inputElement_value ti, domEvent Click nb)

        -- Step 4: summary + POST submission
        rsvpStep_ stepDyn 4 $ mdo
          elAttr "p" ("class" =: "rsvp-step-label") $ text "\161Todo listo!"
          let summaryDyn = summaryRows <$> nameD' <*> guestD' <*> dietaryD'
          elAttr "div" ("class" =: "rsvp-summary") $
            dyn_ $ ffor summaryDyn $ \rows ->
              forM_ rows $ \row -> el "p" $ text row

          let rsvpDyn = Rsvp <$> nameD' <*> guestD' <*> dietaryD'
              reqDyn  = ffor rsvpDyn $ \r ->
                XhrRequest "POST" "/api/rsvp" $ def
                  & xhrRequestConfig_headers     .~ ("Content-Type" =: "application/json")
                  & xhrRequestConfig_sendData    .~ TE.decodeUtf8 (BL.toStrict (encode r))

          (sendBtnEl, _) <- elDynAttr' "button"
            ( ffor statusDyn $ \s ->
                "class" =: "rsvp-btn rsvp-send-btn"
             <> "type"  =: "button"
             <> (if s == StatusSending || s == StatusSuccess
                   then "disabled" =: "disabled" else mempty)
            ) $ dynText (statusBtnLabel <$> statusDyn)
          let sendE = domEvent Click sendBtnEl

          respE <- performRequestAsync (current reqDyn `tag` sendE)
          let resultE = ffor respE $ \resp ->
                case _xhrResponse_status resp of
                  s | s == 200 || s == 204 -> StatusSuccess
                  _                        -> StatusError
          statusDyn <- holdDyn StatusIdle $ leftmost
            [ StatusSending <$ sendE
            , resultE
            ]

          elDynAttr "p"
            ( ffor statusDyn $ \s ->
                "class" =: "rsvp-status"
             <> if statusVisible s then mempty else "style" =: "display:none"
            ) $ dynText (statusMsg <$> statusDyn)

        return (n1E', n2E', n3E', nameD', guestD', dietaryD')

    return (domEvent Click closeBtnEl, leftmost [n1E, n2E, n3E])

  return ()

-- Show a step div only when stepDyn == n; returns whatever the body returns.
rsvpStep :: (DomBuilder t m, PostBuild t m)
         => Dynamic t Int -> Int -> m a -> m a
rsvpStep stepDyn n body =
  elDynAttr "div"
    ( ffor stepDyn $ \s ->
        "class" =: "rsvp-step"
          <> if s == n then mempty else "style" =: "display:none"
    )
    body

-- Version that discards the body's return value.
rsvpStep_ :: (DomBuilder t m, PostBuild t m)
          => Dynamic t Int -> Int -> m a -> m ()
rsvpStep_ stepDyn n body = rsvpStep stepDyn n body >> return ()

-- Build the summary paragraph list shown on step 4.
summaryRows :: Text -> Int -> Text -> [Text]
summaryRows name guests dietary =
  [ "Nombre: "    <> if T.null name then "\8212" else name
  , "Asistentes: " <> T.pack (show guests)
  ] ++ [ "Restricciones: " <> dietary | not (T.null dietary) ]

-- ── RSVP submission status ────────────────────────────────────────────────────

data RsvpStatus = StatusIdle | StatusSending | StatusSuccess | StatusError
  deriving (Eq)

statusBtnLabel :: RsvpStatus -> Text
statusBtnLabel s = case s of
  StatusIdle    -> "Enviar confirmaci\243n \8594"
  StatusSending -> "Enviando\8230"
  StatusSuccess -> "\161Enviado!"
  StatusError   -> "Reintentar"

statusVisible :: RsvpStatus -> Bool
statusVisible StatusIdle = False
statusVisible _          = True

statusMsg :: RsvpStatus -> Text
statusMsg s = case s of
  StatusIdle    -> ""
  StatusSending -> "Enviando confirmaci\243n\8230"
  StatusSuccess -> "\161Confirmaci\243n recibida! Gracias \127881"
  StatusError   -> "Hubo un problema al enviar. Int\233ntalo de nuevo."

-- ── MESA DE REGALOS ──────────────────────────────────────────────────────────

mesaRegalosSection :: (MonadWidget t m, MonadJSM (Performable m)) => m ()
mesaRegalosSection =
  secImage "mesa-regalos" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "images/4.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "MESA DE REGALOS"
      elAttr "div" ("class" =: "h-track" <> "data-reveal" =: "") $
        hCard
          "LIVERPOOL"
          "51981423"
          (Just "https://mesaderegalos.liverpool.com.mx/milistaderegalos/51981423")

hCard :: DomBuilder t m => Text -> Text -> Maybe Text -> m ()
hCard name number mLink =
  elAttr "div" ("class" =: "h-card glass rect registry-card") $ do
    elAttr "p" ("class" =: "mesa-label") $ text name
    elAttr "p" ("class" =: "registry-number") $ text number
    case mLink of
      Nothing  -> blank
      Just url ->
        elAttr "a"
          ( "class" =: "rsvp-btn registry-link-btn"
         <> "href" =: url
         <> "target" =: "_blank"
         <> "rel" =: "noopener noreferrer"
          ) $ text "Ver mesa de regalos"

-- ── VIDEO PARA LOS NOVIOS ─────────────────────────────────────────────────────

videoMsgSection :: DomBuilder t m => m (Event t ())
videoMsgSection =
  secImage "video-mensaje" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "images/5.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "VIDEO PARA LOS NOVIOS"
      elAttr "div" ("class" =: "video-mask" <> "data-reveal" =: "") $
        elAttr "div" ("class" =: "glass rect video-card") $ do
          elAttr "p" ("class" =: "video-msg-text") $
            text "M\225ndale un video corto a los novios"
          (btnEl, _) <- elAttr' "button"
            ( "class" =: "rsvp-btn video-wa-btn"
           <> "type"  =: "button"
           <> "id"    =: "video-upload-open"
            ) $ text "Subir video"
          return (domEvent Click btnEl)

-- ── Video upload popup ───────────────────────────────────────────────────────

videoUploadOverlay :: MonadWidget t m => Event t () -> m ()
videoUploadOverlay openE = mdo
  visibleDyn <- holdDyn False $ leftmost [True <$ openE, False <$ closeE]
  let overlayAttrs = ffor visibleDyn $ \isVisible ->
        "id" =: "video-upload-overlay" <> "class" =: "construction-overlay"
          <> if isVisible then mempty else "style" =: "display:none"

  closeE <- elDynAttr "div" overlayAttrs $ do
    elAttr "div" ("class" =: "construction-backdrop" <> "aria-hidden" =: "true") blank
    elAttr "div"
      ( "class" =: "construction-modal glass rect video-upload-modal"
     <> "role" =: "dialog"
     <> "aria-modal" =: "true"
      ) $ do
      (closeBtnEl, _) <- elAttr' "button"
        ( "class" =: "construction-close"
       <> "type" =: "button"
       <> "aria-label" =: "Cerrar"
        ) $ text "\215"
      elAttr "p" ("class" =: "construction-kicker") $ text "VIDEO"
      elAttr "h3" ("class" =: "construction-title") $ text "Sube tu mensaje"
      elAttr "form" ("id" =: "video-upload-form" <> "class" =: "video-upload-form") $ do
        elAttr "input"
          ( "id" =: "video-upload-name"
         <> "class" =: "rsvp-input"
         <> "name" =: "name"
         <> "type" =: "text"
         <> "placeholder" =: "Tu nombre"
          ) blank
        elAttr "textarea"
          ( "id" =: "video-upload-message"
         <> "class" =: "rsvp-input video-upload-message"
         <> "name" =: "message"
         <> "placeholder" =: "Mensaje opcional"
          ) blank
        elAttr "input"
          ( "id" =: "video-upload-file"
         <> "class" =: "rsvp-input video-upload-file"
         <> "name" =: "video"
         <> "type" =: "file"
         <> "accept" =: "video/*"
          ) blank
        elAttr "p" ("id" =: "video-upload-status" <> "class" =: "rsvp-status") blank
        elAttr "button"
          ( "id" =: "video-upload-submit"
         <> "class" =: "rsvp-btn construction-ok"
         <> "type" =: "submit"
          ) $ text "Enviar video"
      return (domEvent Click closeBtnEl)
  return ()

-- ── Under construction popup ──────────────────────────────────────────────────

underConstructionOverlay :: MonadWidget t m => Event t () -> m ()
underConstructionOverlay openE = mdo
  visibleDyn <- holdDyn False $ leftmost [True <$ openE, False <$ closeE]
  let overlayAttrs = ffor visibleDyn $ \isVisible ->
        "id" =: "under-construction-overlay" <> "class" =: "construction-overlay"
          <> if isVisible then mempty else "style" =: "display:none"

  closeE <- elDynAttr "div" overlayAttrs $ do
    elAttr "div"
      ( "class" =: "construction-backdrop"
     <> "aria-hidden" =: "true"
      ) blank
    elAttr "div"
      ( "class" =: "construction-modal glass rect"
     <> "role" =: "dialog"
     <> "aria-modal" =: "true"
      ) $ do
      (closeBtnEl, _) <- elAttr' "button"
        ( "class" =: "construction-close"
       <> "type" =: "button"
       <> "aria-label" =: "Cerrar"
        ) $ text "\215"
      elAttr "p" ("class" =: "construction-kicker") $ text "AVISO"
      elAttr "h3" ("class" =: "construction-title") $
        text "Website under construction"
      elAttr "p" ("class" =: "construction-copy") $
        text "Estamos afinando esta secci\243n para compartirla pronto."
      (okBtnEl, _) <- elAttr' "button"
        ( "class" =: "rsvp-btn construction-ok"
       <> "type" =: "button"
        ) $ text "Entendido"
      return $ leftmost [domEvent Click closeBtnEl, domEvent Click okBtnEl]

  return ()

-- ── Admin mount ───────────────────────────────────────────────────────────────

adminRoot :: DomBuilder t m => m ()
adminRoot = elAttr "div" ("id" =: "admin-root") blank

-- ── Helpers ───────────────────────────────────────────────────────────────────

sec :: DomBuilder t m => Text -> m a -> m a
sec sid =
  elAttr "section"
    ( "id"    =: sid
   <> "class" =: "section"
    )

secImage :: DomBuilder t m => Text -> m a -> m a
secImage sid =
  elAttr "section"
    ( "id"    =: sid
   <> "class" =: "section image-section"
    )

-- ── All CSS ───────────────────────────────────────────────────────────────────

siteCSS :: Text
siteCSS = T.unlines

  -- Fonts
  [ "@import url('https://fonts.googleapis.com/css2?family=Great+Vibes&family=Courier+Prime:ital,wght@0,400;1,400&display=swap');"
  , ""

  -- ── Reset ─────────────────────────────────────────────────────────────────
  , "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }"
  , "html { scroll-behavior: smooth; }"
  , ":root {"
  , "  --photo-frame-width: min(100vw, max(429px, 71.45svh));"
  , "  --card-width: min(77.2vw, max(331px, 55.2svh));"
  , "  --card-bottom-gap: clamp(4.68rem, 7.2svh, 6.54rem);"
  , "  --card-lift: -15%;"
  , "}"
  , "body {"
  , "  font-family: 'Courier Prime', 'Courier New', monospace;"
  , "  background: #1c1410;"
  , "  color: #f0ebe0;"
  , "  overflow-x: hidden;"
  , "}"
  , "@media (max-width: 640px) {"
  , "  :root {"
  , "    --card-width: min(79.6vw, max(341px, 56.9svh));"
  , "    --card-bottom-gap: clamp(3.54rem, 5.76svh, 4.44rem);"
  , "  }"
  , "}"
  , "@media (orientation: landscape) and (max-height: 500px) {"
  , "  :root {"
  , "    --card-bottom-gap: clamp(1rem, 4svh, 3rem);"
  , "    --card-lift: -5%;"
  , "  }"
  , "  .image-section { height: max(600px, 100svh); }"
  , "  .section-img   { height: max(600px, 100svh); }"
  , "  .section-overlay { padding-top: .6rem; }"
  , "}"
  , "@media (orientation: portrait) and (max-width: 760px) {"
  , "  .section.image-section { overflow: hidden; }"
  , "  .image-section .section-img { width: min(100vw, 390px); height: auto; }"
  , "  #hero .hero-bg {"
  , "    background-size: min(100vw, 390px) auto;"
  , "    background-position: center center;"
  , "  }"
  , "}"
  , "@media (orientation: portrait) and (max-width: 760px) and (max-height: 600px) {"
  , "  .image-section .section-img { width: min(100vw, 390px); }"
  , "  #hero .hero-bg { background-size: min(100vw, 390px) auto; }"
  , "}"
  , ""

  -- ── Section shell ─────────────────────────────────────────────────────────
  , ".section {"
  , "  position: relative;"
  , "  min-height: 100svh;"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  overflow: hidden;"
  , "  isolation: isolate;"
  , "}"
  , ".section::before {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  background: rgba(18,12,5,.20);"
  , "  z-index: -1;"
  , "  pointer-events: none;"
  , "}"
  , ""

  -- ── Image sections ────────────────────────────────────────────────────────
  , ".image-section {"
  , "  position: relative;"
  , "  height: max(600px, 100svh);"
  , "  min-height: unset;"
  , "  display: flex;"
  , "  align-items: safe center;"
  , "  justify-content: safe center;"
  , "  overflow: auto;"
  , "  background: #1c1410;"
  , "  animation: sectionDim linear both;"
  , "  animation-timeline: view();"
  , "  animation-range: exit 20% exit 90%;"
  , "}"
  , "@keyframes sectionDim {"
  , "  from { opacity: 1; }"
  , "  to   { opacity: .22; }"
  , "}"
  , ".image-section::before { display: none; }"
  , ".section-img {"
  , "  height: max(600px, 100svh);"
  , "  width: auto;"
  , "  max-width: none;"
  , "  object-fit: contain;"
  , "  display: block;"
  , "  flex-shrink: 0;"
  , "  will-change: transform;"
  , "  user-select: none;"
  , "  pointer-events: none;"
  , "  animation: imgDrift linear both;"
  , "  animation-timeline: view();"
  , "  animation-range: entry 0% exit 100%;"
  , "}"
  , "@keyframes imgDrift {"
  , "  from { transform: translateY(0); }"
  , "  to   { transform: translateY(-10%); }"
  , "}"
  , ".section-overlay {"
  , "  position: absolute;"
  , "  bottom: 0;"
  , "  left: 0;"
  , "  right: 0;"
  , "  z-index: 2;"
  , "  padding: 1.5rem 1.8rem var(--card-bottom-gap);"
  , "  background: linear-gradient(to top, rgba(28,20,16,.78) 0%, rgba(28,20,16,.30) 65%, transparent 100%);"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  align-items: center;"
  , "}"
  , ""

  -- Section background fallbacks
  , "#hero         { background-color: #3d2e22; }"
  , "#ubicacion    { background-color: #3a2c18; }"
  , "#dress-code   { background-color: #4a3010; }"
  , "#rsvp         { background: radial-gradient(ellipse at 50% 30%, #3a2614 0%, #1c1410 70%); }"
  , "#mesa-regalos { background-color: #382e24; }"
  , "#video-mensaje { background: linear-gradient(180deg, #2c2418 0%, #1a120a 100%); }"
  , ""
  , ".spacer { flex: 1; }"
  , ""

  -- ── Intro overlay (pure CSS timeline) ─────────────────────────────────────
  , ".intro {"
  , "  position: fixed;"
  , "  inset: 0;"
  , "  z-index: 1000;"
  , "  background: #1c1410;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  pointer-events: none;"
  , "  animation: introFadeOut .82s ease-in-out 2.63s forwards;"
  , "}"
  , "@keyframes introFadeOut {"
  , "  from { opacity: 1; visibility: visible; }"
  , "  to   { opacity: 0; visibility: hidden; }"
  , "}"
  , ".intro-inner {"
  , "  text-align: center;"
  , "  padding: 0 2rem;"
  , "  max-width: 560px;"
  , "  width: 100%;"
  , "}"
  , ".intro-kicker {"
  , "  font-family: 'Courier Prime', monospace;"
  , "  font-size: clamp(.68rem, 2.2vw, .95rem);"
  , "  letter-spacing: .28em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.82);"
  , "  line-height: 2;"
  , "  overflow: hidden;"
  , "}"
  , ".intro-word {"
  , "  display: inline-block;"
  , "  opacity: 0;"
  , "  transform: translateY(110%);"
  , "  animation: introWordReveal .74s cubic-bezier(.215,.61,.355,1) forwards;"
  , "  animation-delay: calc(var(--i) * .07s);"
  , "}"
  , "@keyframes introWordReveal {"
  , "  to { opacity: 1; transform: translateY(0); }"
  , "}"
  , ".intro-rule {"
  , "  display: block;"
  , "  height: 1px;"
  , "  width: 0;"
  , "  max-width: 100%;"
  , "  background: #d4b483;"
  , "  margin: 1.2rem auto;"
  , "  animation: introRuleExpand .56s cubic-bezier(.25,.46,.45,.94) .46s forwards;"
  , "}"
  , "@keyframes introRuleExpand {"
  , "  to { width: 60vw; }"
  , "}"
  , ".intro-sign {"
  , "  font-family: 'Great Vibes', cursive;"
  , "  font-size: clamp(2.4rem, 9vw, 4.4rem);"
  , "  color: #fff;"
  , "  line-height: 1.15;"
  , "  opacity: 0;"
  , "  transform: translateY(22px);"
  , "  animation: introSignReveal .7s cubic-bezier(.215,.61,.355,1) .84s forwards;"
  , "}"
  , "@keyframes introSignReveal {"
  , "  to { opacity: 1; transform: translateY(0); }"
  , "}"
  , ""

  -- ── Progress bar — CSS scroll-driven ─────────────────────────────────────
  , ".progress-bar {"
  , "  position: fixed;"
  , "  top: 0; left: 0;"
  , "  width: 100%; height: 2px;"
  , "  background: #d4b483;"
  , "  transform-origin: left center;"
  , "  z-index: 500;"
  , "  pointer-events: none;"
  , "  animation: progressGrow linear both;"
  , "  animation-timeline: scroll();"
  , "}"
  , "@keyframes progressGrow {"
  , "  from { transform: scaleX(0); }"
  , "  to   { transform: scaleX(1); }"
  , "}"
  , ""

  -- ── Hero ──────────────────────────────────────────────────────────────────
  , "#hero::before {"
  , "  background: linear-gradient(180deg, rgba(16,9,4,.08) 0%, rgba(16,9,4,.35) 72%, rgba(16,9,4,.6) 100%);"
  , "}"
  , ".hero-bg {"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  z-index: -2;"
  , "  background-image: url('images/0.png');"
  , "  background-size: auto 100%;"
  , "  background-position: center top;"
  , "  background-repeat: no-repeat;"
  , "  will-change: transform;"
  , "}"
  , ".hero-spacer { flex: 1; }"
  , ".hero-copy {"
  , "  text-align: center;"
  , "  padding: 0 1.2rem 5rem;"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".hero-date {"
  , "  letter-spacing: .2em;"
  , "  font-size: clamp(.7rem, 2.1vw, 1rem);"
  , "  color: rgba(255,255,255,.9);"
  , "  white-space: nowrap;"
  , "  margin-bottom: .45rem;"
  , "  opacity: 0;"
  , "  animation: heroFadeUp .45s ease-out 3.2s forwards;"
  , "}"
  , "@keyframes heroFadeUp {"
  , "  from { opacity: 0; transform: translateY(20px); }"
  , "  to   { opacity: 1; transform: translateY(0); }"
  , "}"
  , "@media (orientation: landscape) and (max-width: 760px) {"
  , "  .hero-bg { background-size: auto 100%; }"
  , "}"
  , ""

  -- ── Fixed bottom navigation ────────────────────────────────────────────────
  , ".fixed-nav {"
  , "  position: fixed;"
  , "  bottom: 0;"
  , "  left: 0;"
  , "  right: 0;"
  , "  z-index: 400;"
  , "  display: flex;"
  , "  justify-content: center;"
  , "  flex-wrap: wrap;"
  , "  gap: .35rem clamp(.85rem, 2.69vw, 2.34rem);"
  , "  padding: clamp(.6rem, 1.90vw, 1.65rem) clamp(1.2rem, 3.80vw, 3.3rem) clamp(.7rem, 2.21vw, 1.93rem);"
  , "  background: rgba(20,13,7,.74);"
  , "  backdrop-filter: blur(24px) saturate(1.2);"
  , "  -webkit-backdrop-filter: blur(24px) saturate(1.2);"
  , "  border-top: 1px solid rgba(255,255,255,.09);"
  , "  animation: navSlideUp .6s ease-out 3.6s both;"
  , "}"
  , "@keyframes navSlideUp {"
  , "  from { opacity: 0; transform: translateY(100%); }"
  , "  to   { opacity: 1; transform: translateY(0); }"
  , "}"
  , ".fixed-nav-link {"
  , "  color: rgba(255,255,255,.65);"
  , "  text-decoration: none;"
  , "  font-size: clamp(.57rem, 1.80vw, 1.57rem);"
  , "  letter-spacing: .22em;"
  , "  text-transform: uppercase;"
  , "  padding: .22rem 0 .18rem;"
  , "  border-bottom: 1.5px solid transparent;"
  , "  transition: color .25s, border-color .25s;"
  , "  white-space: nowrap;"
  , "}"
  , ".fixed-nav-link:hover { color: rgba(255,255,255,.92); }"
  , ".fixed-nav-link.is-active {"
  , "  color: #d4b483;"
  , "  border-bottom-color: #d4b483;"
  , "}"
  , ""

  -- ── Section labels ────────────────────────────────────────────────────────
  , ".label {"
  , "  font-size: min(clamp(1.47rem, 1.44vw, 1.93rem), 6.3svh);"
  , "  letter-spacing: .17em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.87);"
  , "  padding: 1.8rem 1.8rem 0;"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".label-right  { text-align: right; }"
  , ".label-center { text-align: center; }"
  , ""

  -- ── Marquee ───────────────────────────────────────────────────────────────
  , ".marquee {"
  , "  overflow: hidden;"
  , "  white-space: nowrap;"
  , "  padding: .55rem 0;"
  , "  border-bottom: 1px solid rgba(255,255,255,.12);"
  , "  position: relative;"
  , "  z-index: 1;"
  , "  background: rgba(18,12,5,.15);"
  , "}"
  , ".marquee-track {"
  , "  display: inline-block;"
  , "  white-space: nowrap;"
  , "  font-size: .56rem;"
  , "  letter-spacing: .2em;"
  , "  color: rgba(255,255,255,.55);"
  , "  text-transform: uppercase;"
  , "}"
  , ".marquee-track span { margin-right: .2em; }"
  , ".marquee-track { animation: marqueeScroll 30s linear infinite; }"
  , "@keyframes marqueeScroll { to { transform: translateX(-50%); } }"
  , ""

  -- ── Glass cards ───────────────────────────────────────────────────────────
  , ".glass {"
  , "  background: rgba(138,108,76,.10);"
  , "  backdrop-filter: none;"
  , "  -webkit-backdrop-filter: none;"
  , "  border: 1px solid rgba(255,255,255,.13);"
  , "  padding: clamp(1.6rem, 2.8vw, 2.35rem) clamp(1.7rem, 3.1vw, 2.6rem);"
  , "  margin: 1.1rem 1.8rem;"
  , "  line-height: 1.7;"
  , "  font-size: min(clamp(1.29rem, 1.26vw, 1.63rem), 5.5svh);"
  , "  color: rgba(255,255,255,.9);"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".glass p + p { margin-top: .45rem; }"
  , ".blob {"
  , "  border-radius: 44% 56% 38% 62% / 52% 44% 56% 48%;"
  , "  width: calc(100% - 3.6rem);"
  , "  max-width: 420px;"
  , "}"
  , ".rect {"
  , "  border-radius: 14px;"
  , "  width: var(--card-width);"
  , "  max-width: calc(var(--photo-frame-width) - 1.8rem);"
  , "}"
  , ""
  -- These override .glass margin — must come after .glass in the cascade.
  , ".rsvp-confirm {"
  , "  text-align: center;"
  , "  margin: 1.1rem auto;"
  , "}"
  , ".ubicacion-card {"
  , "  text-align: center;"
  , "  margin: 1.1rem auto;"
  , "}"
  , ".rsvp-confirm, .ubicacion-card, .dress-info {"
  , "  transform: translateY(var(--card-lift));"
  , "}"
  , ".map-embed {"
  , "  display: block;"
  , "  width: 100%;"
  , "  height: 220px;"
  , "  border: 0;"
  , "  border-radius: 8px;"
  , "  margin-top: 1rem;"
  , "  opacity: .88;"
  , "}"
  , ""

  -- ── Dress code ────────────────────────────────────────────────────────────
  -- Overlay is top-anchored; 3.png already contains all visual elements.
  , ".dress-code-overlay {"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  bottom: auto;"
  , "  background: linear-gradient(180deg, rgba(22,14,6,.62) 0%, transparent 55%);"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  align-items: center;"
  , "  padding: 2.2rem 1.8rem 0;"
  , "  z-index: 2;"
  , "}"
  , ".dress-info { margin: 1.1rem auto; text-align: center; width: min(90vw, calc(var(--photo-frame-width) - 1.2rem)); }"
  , ".dress-info p + p { white-space: nowrap; }"
  , ""

  -- ── RSVP button (shared by all action buttons) ────────────────────────────
  , ".rsvp-btn {"
  , "  display: inline-block;"
  , "  margin-top: 1.2rem;"
  , "  color: #fff;"
  , "  text-decoration: none;"
  , "  border: 1px solid rgba(255,255,255,.46);"
  , "  border-radius: 4px;"
  , "  padding: .5rem 1.3rem;"
  , "  font-size: clamp(1.1rem, .86vw, 1.27rem);"
  , "  letter-spacing: .1em;"
  , "  font-family: 'Courier Prime', monospace;"
  , "  cursor: pointer;"
  , "  background: none;"
  , "  transition: background .2s, border-color .2s;"
  , "}"
  , ".rsvp-btn:hover { background: rgba(255,255,255,.12); }"
  , ""

  -- ── RSVP overlay ─────────────────────────────────────────────────────────
  , ".rsvp-overlay {"
  , "  position: fixed;"
  , "  inset: 0;"
  , "  z-index: 500;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  background: rgba(18,12,5,.88);"
  , "  backdrop-filter: blur(10px);"
  , "  -webkit-backdrop-filter: blur(10px);"
  , "}"
  , ".rsvp-close {"
  , "  position: absolute;"
  , "  top: 1.2rem;"
  , "  right: 1.5rem;"
  , "  background: none;"
  , "  border: none;"
  , "  color: rgba(255,255,255,.6);"
  , "  font-size: 1.8rem;"
  , "  cursor: pointer;"
  , "  line-height: 1;"
  , "  padding: 0;"
  , "  transition: color .2s;"
  , "}"
  , ".rsvp-close:hover { color: #fff; }"
  , ".rsvp-modal {"
  , "  background: rgba(138,108,76,.38);"
  , "  backdrop-filter: blur(30px) saturate(1.3);"
  , "  -webkit-backdrop-filter: blur(30px) saturate(1.3);"
  , "  border: 1px solid rgba(255,255,255,.18);"
  , "  border-radius: 20px;"
  , "  padding: 2.5rem 2rem 2rem;"
  , "  width: min(90vw, 380px);"
  , "  position: relative;"
  , "  min-height: 220px;"
  , "}"
  , ".rsvp-step { display: block; }"
  , ".rsvp-step-label {"
  , "  font-size: .95rem;"
  , "  letter-spacing: .04em;"
  , "  color: rgba(255,255,255,.92);"
  , "  margin-bottom: 1.3rem;"
  , "  line-height: 1.5;"
  , "}"
  , ".rsvp-input {"
  , "  width: 100%;"
  , "  background: rgba(255,255,255,.10);"
  , "  border: 1px solid rgba(255,255,255,.28);"
  , "  border-radius: 8px;"
  , "  padding: .75rem 1rem;"
  , "  color: #f0ebe0;"
  , "  font-family: 'Courier Prime', monospace;"
  , "  font-size: .87rem;"
  , "  outline: none;"
  , "  margin-bottom: 1.2rem;"
  , "  transition: border-color .2s;"
  , "  box-sizing: border-box;"
  , "}"
  , ".rsvp-input:focus { border-color: rgba(255,255,255,.6); }"
  , ".rsvp-counter {"
  , "  display: flex;"
  , "  align-items: center;"
  , "  gap: 1.8rem;"
  , "  margin: 1rem 0 1.4rem;"
  , "}"
  , ".rsvp-counter-btn {"
  , "  background: rgba(255,255,255,.10);"
  , "  border: 1px solid rgba(255,255,255,.30);"
  , "  border-radius: 50%;"
  , "  width: 2.2rem;"
  , "  height: 2.2rem;"
  , "  color: #f0ebe0;"
  , "  font-size: 1.2rem;"
  , "  cursor: pointer;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  transition: background .2s;"
  , "  line-height: 1;"
  , "  padding: 0;"
  , "  font-family: 'Courier Prime', monospace;"
  , "}"
  , ".rsvp-counter-btn:hover { background: rgba(255,255,255,.22); }"
  , "#rsvp-count {"
  , "  font-size: 2rem;"
  , "  font-family: 'Courier Prime', monospace;"
  , "  color: #fff;"
  , "  min-width: 2rem;"
  , "  text-align: center;"
  , "  display: inline-block;"
  , "}"
  , ".rsvp-summary {"
  , "  margin-bottom: 1.2rem;"
  , "  line-height: 2;"
  , "  font-size: .85rem;"
  , "  color: rgba(255,255,255,.85);"
  , "}"
  , ".rsvp-whatsapp-btn {"
  , "  background: rgba(37,211,102,.16);"
  , "  border-color: rgba(37,211,102,.5);"
  , "}"
  , ".rsvp-whatsapp-btn:hover { background: rgba(37,211,102,.30); }"
  , ""

  -- ── Under construction popup ──────────────────────────────────────────────
  , ".construction-overlay {"
  , "  position: fixed;"
  , "  inset: 0;"
  , "  z-index: 560;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  padding: 1.2rem;"
  , "}"
  , ".construction-backdrop {"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  background: radial-gradient(circle at 30% 20%, rgba(180,139,92,.24), rgba(18,12,5,.92) 58%);"
  , "  backdrop-filter: blur(10px) saturate(1.14);"
  , "  -webkit-backdrop-filter: blur(10px) saturate(1.14);"
  , "}"
  , ".construction-modal {"
  , "  position: relative;"
  , "  z-index: 1;"
  , "  width: min(92vw, 420px);"
  , "  text-align: center;"
  , "  background: rgba(138,108,76,.36);"
  , "  border: 1px solid rgba(255,255,255,.24);"
  , "  border-radius: 22px;"
  , "  box-shadow: 0 18px 60px rgba(0,0,0,.48);"
  , "  padding: 2.15rem 1.5rem 1.65rem;"
  , "  animation: constructionPop .35s cubic-bezier(.19,.86,.26,1) both;"
  , "}"
  , "@keyframes constructionPop {"
  , "  from { opacity: 0; transform: translateY(16px) scale(.96); }"
  , "  to   { opacity: 1; transform: translateY(0) scale(1); }"
  , "}"
  , ".construction-close {"
  , "  position: absolute;"
  , "  top: .8rem;"
  , "  right: .95rem;"
  , "  border: none;"
  , "  background: transparent;"
  , "  color: rgba(255,255,255,.62);"
  , "  font-size: 1.8rem;"
  , "  line-height: 1;"
  , "  cursor: pointer;"
  , "  transition: color .2s;"
  , "}"
  , ".construction-close:hover { color: #fff; }"
  , ".construction-kicker {"
  , "  font-size: .62rem;"
  , "  letter-spacing: .25em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.7);"
  , "}"
  , ".construction-title {"
  , "  margin-top: .55rem;"
  , "  font-size: 1.16rem;"
  , "  letter-spacing: .05em;"
  , "  color: #fff;"
  , "  font-weight: 400;"
  , "}"
  , ".construction-copy {"
  , "  margin-top: .8rem;"
  , "  color: rgba(255,255,255,.84);"
  , "  font-size: .83rem;"
  , "  line-height: 1.8;"
  , "}"
  , ".construction-ok {"
  , "  margin-top: 1.1rem;"
  , "  min-width: 10.5rem;"
  , "}"
  , ""

  -- ── Mesa de Regalos — horizontal track ────────────────────────────────────
  , ".h-track {"
  , "  display: flex;"
  , "  flex-direction: row;"
  , "  flex-wrap: wrap;"
  , "  gap: 2rem;"
  , "  padding: 2rem 3rem;"
  , "  width: 100%;"
  , "  max-width: 940px;"
  , "  margin: 0 auto;"
  , "  justify-content: center;"
  , "  align-items: center;"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".h-card { flex-shrink: 0; margin: 0; }"
  , ".registry-card { line-height: 1.7; text-align: center; }"
  , ".mesa-label {"
  , "  font-size: .72rem;"
  , "  letter-spacing: .27em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.87);"
  , "  margin-bottom: .5rem;"
  , "}"
  , ".registry-number {"
  , "  font-size: 1.5rem;"
  , "  letter-spacing: .12em;"
  , "  color: #fff;"
  , "  margin-bottom: .6rem;"
  , "}"
  , ".registry-link-btn {"
  , "  margin-top: .4rem;"
  , "  font-size: clamp(1.06rem, .83vw, 1.2rem);"
  , "  padding: .44rem 1.05rem;"
  , "}"
  , "@media (max-width: 640px) {"
  , "  .h-track {"
  , "    flex-direction: column;"
  , "    width: 100%;"
  , "    padding: 1rem 1.8rem;"
  , "  }"
  , "  .h-card { min-width: auto; }"
  , "}"
  , ""

  -- ── Video mensaje ─────────────────────────────────────────────────────────
  , ".video-mask { overflow: hidden; }"
  , "#video-mensaje .section-overlay { padding-bottom: calc(var(--card-bottom-gap) * 1.1); }"
  , ".video-card { text-align: center; margin-left: auto; margin-right: auto; }"
  , ".video-msg-icon {"
  , "  display: block;"
  , "  font-size: 2.4rem;"
  , "  margin-bottom: .7rem;"
  , "  line-height: 1;"
  , "}"
  , ".video-msg-text {"
  , "  font-size: 1em;"
  , "  color: rgba(255,255,255,.85);"
  , "  line-height: 1.72;"
  , "  margin-bottom: .8rem;"
  , "}"
  , ".video-wa-btn { margin-top: .8rem; }"
  , ".video-upload-form { margin-top: 1.1rem; text-align: left; }"
  , ".video-upload-message { min-height: 6rem; resize: vertical; }"
  , ".video-upload-file { padding: .62rem; }"
  , ".rsvp-status.is-error { color: #ffb4a8; }"
  , ""

  -- ── Admin dashboard ───────────────────────────────────────────────────────
  , "#admin-root { display: none; min-height: 100svh; background: #160f0a; }"
  , "body.admin-mode { background: #160f0a; overflow-x: hidden; }"
  , "body.admin-mode .site-shell { display: none; }"
  , "body.admin-mode #admin-root { display: block; }"
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
  , ""

  -- ── [data-reveal] — scroll-driven reveal (visible by default for Safari) ──
  , "[data-reveal] { opacity: 1; transform: none; }"
  , "@supports (animation-timeline: view()) {"
  , "  [data-reveal] {"
  , "    opacity: 0;"
  , "    transform: translateY(26px);"
  , "    animation: revealIn .7s ease-out both;"
  , "    animation-timeline: view();"
  , "    animation-range: entry 10% entry 45%;"
  , "  }"
  , "  @keyframes revealIn {"
  , "    from { opacity: 0; transform: translateY(26px); }"
  , "    to   { opacity: 1; transform: translateY(0); }"
  , "  }"
  , "}"
  , ""

  -- ── Back to top ───────────────────────────────────────────────────────────
  , ".back-to-top {"
  , "  position: fixed;"
  , "  bottom: 5rem;"
  , "  right: 1.6rem;"
  , "  z-index: 300;"
  , "  width: 3rem;"
  , "  height: 3rem;"
  , "  border-radius: 50%;"
  , "  background: rgba(138,108,76,.28);"
  , "  border: 1px solid rgba(255,255,255,.22);"
  , "  cursor: pointer;"
  , "  padding: 0;"
  , "  display: block;"
  , "  text-decoration: none;"
  , "  transition: background .25s, border-color .25s, transform .25s;"
  , "  box-shadow: 0 4px 24px rgba(0,0,0,.35);"
  , "}"
  , ".back-to-top:hover {"
  , "  background: rgba(138,108,76,.52);"
  , "  border-color: rgba(255,255,255,.5);"
  , "  transform: translateY(-3px);"
  , "}"
  , ".back-to-top:active { transform: translateY(0); }"
  , ".back-to-top::before,"
  , ".back-to-top::after {"
  , "  content: '';"
  , "  position: absolute;"
  , "  top: 50%;"
  , "  width: .7rem;"
  , "  height: 1.5px;"
  , "  background: rgba(255,255,255,.88);"
  , "  border-radius: 2px;"
  , "}"
  , ".back-to-top::before {"
  , "  left: calc(50% - .62rem);"
  , "  transform: translateY(-35%) rotate(-42deg);"
  , "}"
  , ".back-to-top::after {"
  , "  left: calc(50% - .08rem);"
  , "  transform: translateY(-35%) rotate(42deg);"
  , "}"
  , ""

  -- ── Reduced motion ────────────────────────────────────────────────────────
  , "@media (prefers-reduced-motion: reduce) {"
  , "  .intro { display: none !important; }"
  , "  .progress-bar { display: none; }"
  , "  .intro-word, .intro-rule, .intro-sign { animation: none; opacity: 1; transform: none; }"
  , "  .hero-date { animation: none; opacity: 1; transform: none; }"
  , "  .marquee-track { animation: none; }"
  , "  [data-reveal] { animation: none !important; opacity: 1; transform: none; }"
  , "  .fixed-nav { opacity: 1; transform: none; }"
  , "}"
  ]
