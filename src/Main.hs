{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Control.Monad (forM_, void)
import Language.Javascript.JSaddle (eval, MonadJSM, liftJSM)
import Reflex.Dom

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
  introOverlay
  progressBar
  heroSection
  openRsvpE <- rsvpSection
  videoMsgSection
  ubicacionSection
  dressCodeSection
  mesaRegalosSection
  fixedNav
  rsvpOverlay openRsvpE
  backToTop
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
      , ("#video-mensaje", "VIDEO")
      , ("#ubicacion",     "UBICACI\211N")
      , ("#dress-code",    "DRESS CODE")
      , ("#mesa-regalos",  "REGALOS")
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

rsvpSection :: (DomBuilder t m, MonadHold t m, PostBuild t m) => m (Event t ())
rsvpSection =
  secImage "rsvp" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "images/1.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    openE <- elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $ text "RSVP"
      e <- elAttr "div" ("class" =: "glass rect rsvp-confirm" <> "data-reveal" =: "") $ do
        el "p" $ text "Por favor confirma tu asistencia"
        el "p" $ text "antes del 10 de septiembre de 2026."
        (btnEl, _) <- elAttr' "button" ("class" =: "rsvp-btn") $ text "Confirmar \8594"
        return (() <$ domEvent Click btnEl)
      return e
    return openE

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

        -- Step 4: summary + WhatsApp link (no next button)
        rsvpStep_ stepDyn 4 $ do
          elAttr "p" ("class" =: "rsvp-step-label") $ text "\161Todo listo!"
          let summaryDyn = summaryRows <$> nameD' <*> guestD' <*> dietaryD'
          elAttr "div" ("class" =: "rsvp-summary") $
            dyn_ $ ffor summaryDyn $ \rows ->
              forM_ rows $ \row -> el "p" $ text row
          let hrefDyn = buildWaHref <$> nameD' <*> guestD' <*> dietaryD'
          elDynAttr "a"
            ( ffor hrefDyn $ \h ->
                "class"  =: "rsvp-btn rsvp-whatsapp-btn"
             <> "href"   =: h
             <> "target" =: "_blank"
             <> "rel"    =: "noopener noreferrer"
            ) $ text "Enviar por WhatsApp \8594"

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

-- Build the wa.me href with percent-encoded message.
buildWaHref :: Text -> Int -> Text -> Text
buildWaHref name guests dietary =
  "https://wa.me/PLACEHOLDER?text=" <> percentEncode msg
  where
    msg = T.intercalate "\n" $
      [ "\xa1Hola! Confirmo mi asistencia a la boda de Daniel y Ana Cristina \127881"
      , "Nombre: "     <> if T.null name then "\8212" else name
      , "Asistentes: " <> T.pack (show guests)
      ] ++ [ "Restricciones: " <> dietary | not (T.null dietary) ]

-- RFC-3986 percent-encoding (UTF-8 bytes, unreserved chars pass through).
percentEncode :: Text -> Text
percentEncode = T.pack . concatMap encByte . BS.unpack . TE.encodeUtf8
  where
    encByte :: Word8 -> String
    encByte b
      | (b >= 65 && b <= 90)   -- A-Z
        || (b >= 97 && b <= 122)  -- a-z
        || (b >= 48 && b <= 57)   -- 0-9
        || b == 45 || b == 95 || b == 46 || b == 126  -- - _ . ~
        = [toEnum (fromIntegral b)]
      | otherwise = '%' : [hexDig (b `div` 16), hexDig (b `mod` 16)]
    hexDig d = if d < 10
               then toEnum (fromIntegral d + 48)   -- '0'
               else toEnum (fromIntegral d - 10 + 65)  -- 'A'

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
        hCard "LIVERPOOL" "51981423" (Just "51981423")

hCard :: (MonadWidget t m, MonadJSM (Performable m)) => Text -> Text -> Maybe Text -> m ()
hCard name number mCopy =
  elAttr "div" ("class" =: "h-card glass rect registry-card") $ do
    elAttr "p" ("class" =: "mesa-label") $ text name
    elAttr "p" ("class" =: "registry-number") $ text number
    case mCopy of
      Nothing  -> blank
      Just val -> copyButton val

-- Button that writes val to the clipboard and temporarily shows "¡Copiado!".
copyButton :: (MonadWidget t m, MonadJSM (Performable m)) => Text -> m ()
copyButton val = mdo
  resetE  <- delay 1.4 clickE
  labelDyn <- holdDyn "Copiar" $ leftmost
    [ "\xa1Copiado!" <$ clickE
    , "Copiar"       <$ resetE
    ]
  (btnEl, _) <- elAttr' "button" ("class" =: "rsvp-btn copy-btn") $
    dynText labelDyn
  let clickE = domEvent Click btnEl
  performEvent_ $ ffor clickE $ \_ ->
    liftJSM $ void $ eval ("navigator.clipboard.writeText('" <> T.unpack val <> "'")

-- ── VIDEO PARA LOS NOVIOS ─────────────────────────────────────────────────────

videoMsgSection :: DomBuilder t m => m ()
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
          elAttr "span" ("class" =: "video-msg-icon") $ text "\127916"
          elAttr "p" ("class" =: "video-msg-text") $
            text "Gr\225banos un video corto deseando lo mejor a los novios \
                 \y env\237anoslo por WhatsApp."
          elAttr "a"
            ( "class"        =: "rsvp-btn video-wa-btn"
           <> "href"         =: "https://wa.me/PLACEHOLDER?text=\
                                \Video%20para%20Daniel%20y%20Ana%20Cristina%20\
                                \%F0%9F%8E%AC"
           <> "target" =: "_blank"
           <> "rel"    =: "noopener noreferrer"
            ) $ text "Enviar video \8594"

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
  , "body {"
  , "  font-family: 'Courier Prime', 'Courier New', monospace;"
  , "  background: #1c1410;"
  , "  color: #f0ebe0;"
  , "  overflow-x: hidden;"
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
  , "  height: 100svh;"
  , "  min-height: unset;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  overflow: hidden;"
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
  , "  height: 100svh;"
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
  , "  padding: 1.5rem 1.8rem 4.5rem;"
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
  , "@media (max-width: 760px) {"
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
  , "  gap: .35rem .85rem;"
  , "  padding: .6rem 1.2rem .7rem;"
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
  , "  font-size: .57rem;"
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
  , "  font-size: .63rem;"
  , "  letter-spacing: .27em;"
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
  , "  padding: 1.8rem 2.2rem;"
  , "  margin: 1.1rem 1.8rem;"
  , "  line-height: 2;"
  , "  font-size: .87rem;"
  , "  color: rgba(255,255,255,.9);"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".blob {"
  , "  border-radius: 44% 56% 38% 62% / 52% 44% 56% 48%;"
  , "  width: calc(100% - 3.6rem);"
  , "  max-width: 420px;"
  , "}"
  , ".rect { border-radius: 14px; max-width: 320px; }"
  , ""
  -- These override .glass margin — must come after .glass in the cascade.
  , ".rsvp-confirm {"
  , "  text-align: center;"
  , "  margin: 1.1rem auto;"
  , "  width: calc(100% - 3.6rem);"
  , "}"
  , ".ubicacion-card {"
  , "  text-align: center;"
  , "  margin: 1.1rem auto;"
  , "  max-width: 380px;"
  , "  width: calc(100% - 3.6rem);"
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
  , ".dress-info { margin: 1.1rem auto; text-align: center; }"
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
  , "  font-size: .73rem;"
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
  , ".h-card {"
  , "  min-width: 260px;"
  , "  max-width: 320px;"
  , "  flex-shrink: 0;"
  , "  margin: 0;"
  , "}"
  , ".registry-card { line-height: 1.7; }"
  , ".mesa-label {"
  , "  font-size: .63rem;"
  , "  letter-spacing: .27em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.87);"
  , "  margin-bottom: .5rem;"
  , "}"
  , ".registry-number {"
  , "  font-size: 1.3rem;"
  , "  letter-spacing: .12em;"
  , "  color: #fff;"
  , "  margin-bottom: .6rem;"
  , "}"
  , ".copy-btn {"
  , "  margin-top: .4rem;"
  , "  font-size: .65rem;"
  , "  padding: .32rem .9rem;"
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
  , ".video-card { max-width: 320px; text-align: center; margin-left: auto; margin-right: auto; }"
  , ".video-msg-icon {"
  , "  display: block;"
  , "  font-size: 2.4rem;"
  , "  margin-bottom: .7rem;"
  , "  line-height: 1;"
  , "}"
  , ".video-msg-text {"
  , "  font-size: .85rem;"
  , "  color: rgba(255,255,255,.85);"
  , "  line-height: 1.8;"
  , "  margin-bottom: .8rem;"
  , "}"
  , ".video-wa-btn { margin-top: .8rem; }"
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
