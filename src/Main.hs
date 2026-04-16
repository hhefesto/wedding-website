{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Reflex.Dom

-- ── Entry point ───────────────────────────────────────────────────────────────

main :: IO ()
main = mainWidgetWithHead headW bodyW

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Daniel y Ana Cristina — 10 · 10 · 26"
  el "style" $ text siteCSS

-- ── Body ──────────────────────────────────────────────────────────────────────

bodyW :: DomBuilder t m => m ()
bodyW = do
  introOverlay
  progressBar
  heroSection
  collageSection
  rsvpSection
  videoMsgSection
  ubicacionSection
  dressCodeSection
  mesaRegalosSection
  rsvpOverlay
  backToTop

-- ── Intro overlay ─────────────────────────────────────────────────────────────
-- Full-screen panel that plays the invitation text then clip-path reveals hero.

introOverlay :: DomBuilder t m => m ()
introOverlay =
  elAttr "div" ("id" =: "intro" <> "class" =: "intro") $
    elAttr "div" ("class" =: "intro-inner") $ do
      elAttr "p" ("class" =: "intro-kicker") $
        text "Te invitamos a nuestra boda"
      elAttr "span" ("class" =: "intro-rule") blank
      elAttr "p" ("class" =: "intro-sign") $
        text "atte. Cristy y Daniel"

-- ── Progress bar ──────────────────────────────────────────────────────────────

progressBar :: DomBuilder t m => m ()
progressBar =
  elAttr "div" ("id" =: "progress-bar" <> "class" =: "progress-bar") blank

-- ── Collage hub ──────────────────────────────────────────────────────────────

collageSection :: DomBuilder t m => m ()
collageSection =
  elAttr "section" ("id" =: "collage" <> "class" =: "section collage-section") $ do
    elAttr "div" ("class" =: "collage-heading") $ do
      elAttr "p" ("class" =: "collage-kicker") $ text "Te invitamos"
      elAttr "h1" ("class" =: "collage-title") $ text "Ana Cristina y Daniel"
      elAttr "p" ("class" =: "collage-subtitle") $
        text "Cada imagen abre una parte de nuestra boda"
    elAttr "div" ("class" =: "collage-grid") $ do
      collageCard "#rsvp"         "RSVP"         "images/1.png"
      collageCard "#video-mensaje" "VIDEO"       "images/5.png"
      collageCard "#ubicacion"    "UBICACION"    "images/2.png"
      collageCard "#dress-code"   "DRESS CODE"   "images/3.png"
      collageCard "#mesa-regalos" "MESA REGALOS" "images/4.png"

collageCard :: DomBuilder t m => Text -> Text -> Text -> m ()
collageCard href label src =
  elAttr "a"
    ( "class" =: "collage-card"
   <> "href" =: href
   <> "data-magnetic" =: ""
    ) $ do
    elAttr "img"
      ( "src" =: src
     <> "alt" =: label
     <> "loading" =: "lazy"
      ) blank
    elAttr "span" ("class" =: "collage-label") $ text label

-- ── Back to top ──────────────────────────────────────────────────────────────

backToTop :: DomBuilder t m => m ()
backToTop =
  elAttr "button"
    ( "id"         =: "back-to-top"
   <> "class"      =: "back-to-top"
   <> "aria-label" =: "Volver arriba"
   <> "style"      =: "display:none"
    ) blank

-- ── HERO ─────────────────────────────────────────────────────────────────────
-- Clean text-free photo (images/1.png) as inner .hero-bg layer.
-- Live typography is the sole source of names/date.

heroSection :: DomBuilder t m => m ()
heroSection =
  sec "hero" $ do
    elAttr "div" ("class" =: "hero-bg") blank
    elAttr "div" ("class" =: "hero-spacer") blank
    elAttr "div" ("class" =: "hero-copy") $
      elAttr "p" ("class" =: "hero-date") $ text "10/10/26"
    elAttr "nav" ("class" =: "hero-nav" <> "aria-label" =: "Secciones") $ do
      navA "#ubicacion"     "UBICACI\211N"
      navA "#dress-code"    "DRESS CODE"
      navA "#rsvp"          "RSVP"
      navA "#mesa-regalos"  "MESA DE REGALOS"
      navA "#video-mensaje" "VIDEO"

-- ── UBICACIÓN ────────────────────────────────────────────────────────────────
-- images/2.png (couple in forest path). Marquee ticker at top.

ubicacionSection :: DomBuilder t m => m ()
ubicacionSection =
  secZoom "ubicacion" "65% 12%" $ do
    elAttr "div" ("class" =: "section-photo ubicacion-bg") blank
    elAttr "div" ("class" =: "section-content") $ do
      elAttr "div"
        ("class" =: "marquee" <> "aria-hidden" =: "true" <> "data-reveal" =: "") $
        elAttr "div" ("class" =: "marquee-track") $
          forM_ [(1::Int)..8] $ \_ ->
            el "span" $ text
              "VISTA REAL \xb7 GRAN TERRAZA \xb7 6 PM \xb7 10.10.26 \xa0\xa0"
      elAttr "p" ("class" =: "label label-right" <> "data-reveal" =: "") $
        text "UBICACI\211N"
      elAttr "div" ("class" =: "glass blob" <> "data-reveal" =: "") $ do
        el "p" $ text "Gran Terraza"
        el "p" $ text "Vista Real Country Club"
        el "p" $ text "6 pm"
      elAttr "div" ("class" =: "spacer") blank

-- ── DRESS CODE ───────────────────────────────────────────────────────────────
-- images/3.png backdrop + suits (6.png) and gowns (7.png) cutout runway.
-- Pinned clip-path zoom animation reveals everything on scroll.

dressCodeSection :: DomBuilder t m => m ()
dressCodeSection =
  secZoom "dress-code" "22% 68%" $ do
    elAttr "div" ("class" =: "section-photo dress-bg") blank
    elAttr "div" ("class" =: "section-content") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "DRESS CODE"
      elAttr "div" ("class" =: "glass rect dress-info" <> "data-reveal" =: "") $ do
        el "p" $ text "Formal"
        el "p" $ text "H: traje y corbata"
        el "p" $ text "M: corto, midi, largo"
      elAttr "div" ("class" =: "dress-runway" <> "data-reveal" =: "") $ do
        elAttr "img"
          ( "class" =: "dress-cutout dress-suits"
         <> "src"   =: "images/6.png"
         <> "alt"   =: ""
          ) blank
        elAttr "img"
          ( "class" =: "dress-cutout dress-gowns"
         <> "src"   =: "images/7.png"
         <> "alt"   =: ""
          ) blank
      elAttr "div" ("class" =: "spacer") blank

-- ── RSVP ─────────────────────────────────────────────────────────────────────

rsvpSection :: DomBuilder t m => m ()
rsvpSection =
  secZoom "rsvp" "8% 16%" $ do
    elAttr "div" ("class" =: "section-photo rsvp-bg") blank
    elAttr "div" ("class" =: "section-content") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $ text "RSVP"
      elAttr "div" ("class" =: "glass rect" <> "data-reveal" =: "") $ do
        el "p" $ text "Por favor confirma tu asistencia"
        el "p" $ text "antes del 10 de septiembre de 2026."
        elAttr "button"
          ( "class"        =: "rsvp-btn"
         <> "id"           =: "rsvp-open-btn"
         <> "data-magnetic" =: ""
          ) $ text "Confirmar \8594"
      elAttr "div" ("class" =: "spacer") blank

-- RSVP multi-step WhatsApp overlay (fixed, outside sections)
rsvpOverlay :: DomBuilder t m => m ()
rsvpOverlay =
  elAttr "div"
    ( "id"    =: "rsvp-overlay"
   <> "class" =: "rsvp-overlay"
   <> "style" =: "display:none"
    ) $ do
    elAttr "button" ("id" =: "rsvp-close" <> "class" =: "rsvp-close") $
      text "\215"
    elAttr "div" ("class" =: "rsvp-modal") $ do
      -- Step 1: Name
      elAttr "div" ("class" =: "rsvp-step" <> "id" =: "rsvp-step-1") $ do
        elAttr "p" ("class" =: "rsvp-step-label") $
          text "\191C\243mo te llamas?"
        elAttr "input"
          ( "type"        =: "text"
         <> "id"          =: "rsvp-name"
         <> "class"       =: "rsvp-input"
         <> "placeholder" =: "Tu nombre completo"
          ) blank
        elAttr "button"
          ("class" =: "rsvp-btn rsvp-next" <> "data-next" =: "2") $
          text "Continuar \8594"
      -- Step 2: Guest count
      elAttr "div"
        ("class" =: "rsvp-step" <> "id" =: "rsvp-step-2"
         <> "style" =: "display:none") $ do
        elAttr "p" ("class" =: "rsvp-step-label") $
          text "\191Cu\225ntos asistir\225n?"
        elAttr "div" ("class" =: "rsvp-counter") $ do
          elAttr "button"
            ("class" =: "rsvp-counter-btn" <> "id" =: "rsvp-minus") $
            text "\8722"
          elAttr "span" ("id" =: "rsvp-count") $ text "1"
          elAttr "button"
            ("class" =: "rsvp-counter-btn" <> "id" =: "rsvp-plus") $
            text "+"
        elAttr "button"
          ("class" =: "rsvp-btn rsvp-next" <> "data-next" =: "3") $
          text "Continuar \8594"
      -- Step 3: Dietary restrictions
      elAttr "div"
        ("class" =: "rsvp-step" <> "id" =: "rsvp-step-3"
         <> "style" =: "display:none") $ do
        elAttr "p" ("class" =: "rsvp-step-label") $
          text "\191Alguna restricci\243n alimentaria?"
        elAttr "input"
          ( "type"        =: "text"
         <> "id"          =: "rsvp-dietary"
         <> "class"       =: "rsvp-input"
         <> "placeholder" =: "Opcional"
          ) blank
        elAttr "button"
          ("class" =: "rsvp-btn rsvp-next" <> "data-next" =: "4") $
          text "Continuar \8594"
      -- Step 4: Summary + WhatsApp send
      elAttr "div"
        ("class" =: "rsvp-step" <> "id" =: "rsvp-step-4"
         <> "style" =: "display:none") $ do
        elAttr "p" ("class" =: "rsvp-step-label") $
          text "\161Todo listo!"
        elAttr "div"
          ("id" =: "rsvp-summary" <> "class" =: "rsvp-summary") blank
        elAttr "a"
          ( "id"     =: "rsvp-whatsapp-btn"
         <> "class"  =: "rsvp-btn rsvp-whatsapp-btn"
         <> "href"   =: "#"
         <> "target" =: "_blank"
         <> "rel"    =: "noopener noreferrer"
          ) $ text "Enviar por WhatsApp \8594"

-- ── MESA DE REGALOS ──────────────────────────────────────────────────────────
-- images/4.png backdrop. Horizontal scroll track.

mesaRegalosSection :: DomBuilder t m => m ()
mesaRegalosSection =
  secZoom "mesa-regalos" "86% 76%" $ do
    elAttr "div" ("class" =: "section-photo mesa-bg") blank
    elAttr "div" ("class" =: "section-content") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "MESA DE REGALOS"
      elAttr "div" ("class" =: "h-track" <> "data-reveal" =: "") $ do
        hCard "LIVERPOOL" "51981423" "51981423"
        hCard "PR\211XIMAMENTE" "\8212" ""
      elAttr "div" ("class" =: "spacer") blank

hCard :: DomBuilder t m => Text -> Text -> Text -> m ()
hCard name number copyVal =
  elAttr "div" ("class" =: "h-card glass rect registry-card") $ do
    elAttr "p" ("class" =: "mesa-label") $ text name
    elAttr "p" ("class" =: "registry-number") $ text number
    if T.null copyVal
      then blank
      else elAttr "button"
             ( "class"     =: "rsvp-btn copy-btn"
            <> "data-copy" =: copyVal
             ) $ text "Copiar"

-- ── VIDEO PARA LOS NOVIOS ─────────────────────────────────────────────────────

videoMsgSection :: DomBuilder t m => m ()
videoMsgSection =
  secZoom "video-mensaje" "80% 22%" $ do
    elAttr "div" ("class" =: "section-photo video-bg") blank
    elAttr "div" ("class" =: "section-content") $ do
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
           <> "target"       =: "_blank"
           <> "rel"          =: "noopener noreferrer"
           <> "data-magnetic" =: ""
            ) $ text "Enviar video \8594"
      elAttr "div" ("class" =: "spacer") blank

-- ── CLOSING ──────────────────────────────────────────────────────────────────
-- images/5.png (couple sunlit forest). Split-text reveal + countdown.

closingSection :: DomBuilder t m => m ()
closingSection =
  sec "closing" $ do
    elAttr "div" ("class" =: "closing-bg") blank
    elAttr "div" ("class" =: "spacer") blank
    elAttr "div" ("class" =: "closing-text") $ do
      elAttr "span" ("class" =: "closing-line") $ text "\xa1Nos vemos el"
      elAttr "span" ("class" =: "closing-line") $ text "10 de octubre!"
    elAttr "div" ("class" =: "countdown" <> "aria-live" =: "polite") $ do
      el "span" $ text "Faltan "
      elAttr "span" ("id" =: "countdown-days") $ text "\8212"
      el "span" $ text " d\237as"

-- ── Helpers ───────────────────────────────────────────────────────────────────

sec :: DomBuilder t m => Text -> m () -> m ()
sec sid =
  elAttr "section"
    ( "id"    =: sid
   <> "class" =: "section"
    )

secZoom :: DomBuilder t m => Text -> Text -> m () -> m ()
secZoom sid zoomOrigin =
  elAttr "section"
    ( "id"               =: sid
   <> "class"            =: "section zoom-section"
   <> "data-zoom-origin" =: zoomOrigin
    )

navA :: DomBuilder t m => Text -> Text -> m ()
navA href label =
  elAttr "a" ("href" =: href <> "data-magnetic" =: "") $ text label

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

  -- ── Split-text helpers ────────────────────────────────────────────────────
  , ".sw { display: inline-block; overflow: hidden; vertical-align: bottom; }"
  , ".sw-i { display: inline-block; }"
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
  , ".zoom-section {"
  , "  min-height: 100svh;"
  , "  position: relative;"
  , "}"
  , ".section-content {"
  , "  min-height: 100svh;"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ""
  , "#collage {"
  , "  background: radial-gradient(circle at 18% 20%, #4a3727 0%, #1d140d 68%);"
  , "  padding: clamp(1.5rem, 4vw, 2.8rem) clamp(1rem, 3vw, 2.6rem);"
  , "}"
  , ".collage-heading {"
  , "  text-align: center;"
  , "  margin-bottom: 1.4rem;"
  , "}"
  , ".collage-kicker {"
  , "  letter-spacing: .22em;"
  , "  text-transform: uppercase;"
  , "  font-size: .62rem;"
  , "  color: rgba(255,255,255,.72);"
  , "}"
  , ".collage-title {"
  , "  margin-top: .4rem;"
  , "  font-family: 'Great Vibes', cursive;"
  , "  font-weight: 400;"
  , "  line-height: .92;"
  , "  font-size: clamp(3rem, 11vw, 6.2rem);"
  , "}"
  , ".collage-subtitle {"
  , "  margin-top: .6rem;"
  , "  color: rgba(255,255,255,.75);"
  , "  font-size: .74rem;"
  , "  letter-spacing: .06em;"
  , "}"
  , ".collage-grid {"
  , "  display: grid;"
  , "  grid-template-columns: repeat(2, minmax(0, 280px));"
  , "  justify-content: center;"
  , "  gap: clamp(.75rem, 1.8vw, 1.2rem);"
  , "}"
  , ".collage-card {"
  , "  position: relative;"
  , "  border-radius: 14px;"
  , "  overflow: hidden;"
  , "  aspect-ratio: 3 / 4.7;"
  , "  min-height: clamp(280px, 44vw, 500px);"
  , "  border: 1px solid rgba(255,255,255,.18);"
  , "  text-decoration: none;"
  , "  color: #fff;"
  , "  box-shadow: 0 14px 34px rgba(0,0,0,.35);"
  , "}"
  , ".collage-card img {"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  width: 100%;"
  , "  height: 100%;"
  , "  object-fit: cover;"
  , "  object-position: center;"
  , "  transition: transform .55s cubic-bezier(.2,.75,.2,1);"
  , "}"
  , ".collage-card::after {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  background: linear-gradient(180deg, rgba(0,0,0,.05) 10%, rgba(0,0,0,.55) 100%);"
  , "}"
  , ".collage-card:hover img { transform: scale(1.06); }"
  , ".collage-label {"
  , "  position: absolute;"
  , "  left: .9rem;"
  , "  bottom: .8rem;"
  , "  z-index: 1;"
  , "  letter-spacing: .16em;"
  , "  text-transform: uppercase;"
  , "  font-size: .61rem;"
  , "}"
  , ".collage-card.is-active {"
  , "  border-color: rgba(255,255,255,.88);"
  , "  box-shadow: 0 0 0 1px rgba(255,255,255,.46), 0 14px 34px rgba(0,0,0,.35);"
  , "}"
  , "@media (max-width: 700px) {"
  , "  .collage-grid { grid-template-columns: 1fr; }"
  , "}"
  , ""
  -- Subtle warm scrim — sits above inner bg layers (z:-2), below content (z:0)
  , ".section::before {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  background: rgba(18,12,5,.20);"
  , "  z-index: -1;"
  , "  pointer-events: none;"
  , "}"
  , "#collage::before { background: transparent; }"
  , ".zoom-section::before { background: rgba(16,10,6,.30); }"
  -- Dress-code: stronger amber overlay
  , "#dress-code::before { background: rgba(52,28,4,.55); }"
  , "#rsvp::before, #video-mensaje::before { background: rgba(16,10,6,.38); }"
  , ""

  -- ── Inner background layers ───────────────────────────────────────────────
  -- z:-2 puts them below the ::before scrim at z:-1.
  -- overflow:hidden on .section clips any scale/transform overflow.
  , ".section-photo, .hero-bg, .ubicacion-bg, .dress-bg, .rsvp-bg, .mesa-bg, .video-bg, .closing-bg {"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  z-index: -2;"
  , "  background-size: cover;"
  , "  background-position: center;"
  , "  background-repeat: no-repeat;"
  , "  will-change: transform;"
  , "}"
  , ".hero-bg      { background-image: url('images/1.png'); }"
  , ".ubicacion-bg { background-image: url('images/2.png'); background-position: center top; }"
  , ".dress-bg     { background-image: url('images/3.png'); }"
  , ".rsvp-bg      { background-image: url('images/1.png'); background-position: center 34%; }"
  , ".mesa-bg      { background-image: url('images/4.png'); background-position: center 40%; }"
  , ".video-bg     { background-image: url('images/5.png'); background-position: center 24%; }"
  , ".closing-bg   { background-image: url('images/5.png'); background-position: center top; }"
  , ".zoom-section .section-photo {"
  , "  transform: scale(1.18);"
  , "  clip-path: inset(18% 10% 18% 10% round 22px);"
  , "}"
  , "[data-reveal] {"
  , "  opacity: 0;"
  , "  transform: translateY(26px);"
  , "}"
  , ""
  -- Section color fallbacks (shown before image loads or when no image)
  , "#collage      { background-color: #24170f; }"
  , "#hero         { background-color: #3d2e22; }"
  , "#ubicacion    { background-color: #2e3a28; }"
  , "#dress-code   { background-color: #4a3010; }"
  , "#rsvp         { background: radial-gradient(ellipse at 50% 30%, #3a2614 0%, #1c1410 70%); }"
  , "#mesa-regalos { background-color: #382e24; }"
  , "#video-mensaje { background: linear-gradient(180deg, #2c2418 0%, #1a120a 100%); }"
  , "#closing      { background-color: #4a3220; }"
  , ""
  -- Flex spacer
  , ".spacer { flex: 1; }"
  , ""

  -- ── Intro overlay ─────────────────────────────────────────────────────────
  , ".intro {"
  , "  position: fixed;"
  , "  inset: 0;"
  , "  z-index: 1000;"
  , "  background: #1c1410;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
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
  , "  overflow: visible;"  -- overflow on .sw handles the clip
  , "}"
  , ".intro-rule {"
  , "  display: block;"
  , "  height: 1px;"
  , "  width: 0;"
  , "  background: #d4b483;"
  , "  margin: 1.2rem auto;"
  , "}"
  , ".intro-sign {"
  , "  font-family: 'Great Vibes', cursive;"
  , "  font-size: clamp(2.4rem, 9vw, 4.4rem);"
  , "  color: #fff;"
  , "  line-height: 1.15;"
  , "  opacity: 0;"  -- GSAP reveals this
  , "}"
  , ""

  -- ── Progress bar ──────────────────────────────────────────────────────────
  , ".progress-bar {"
  , "  position: fixed;"
  , "  top: 0; left: 0;"
  , "  width: 100%; height: 2px;"
  , "  background: #d4b483;"
  , "  transform-origin: left center;"
  , "  transform: scaleX(0);"
  , "  z-index: 500;"
  , "  pointer-events: none;"
  , "}"
  , ""

  -- ── Hero ──────────────────────────────────────────────────────────────────
  , "#hero::before {"
  , "  background: linear-gradient(180deg, rgba(16,9,4,.08) 0%, rgba(16,9,4,.35) 72%, rgba(16,9,4,.6) 100%);"
  , "}"
  , ".hero-bg {"
  , "  background-image: url('images/0.png');"
  , "  background-size: auto 100%;"
  , "  background-position: center top;"
  , "}"
  , ".hero-spacer { flex: 1; }"
  , ".hero-copy {"
  , "  text-align: center;"
  , "  padding: 0 1.2rem 1rem;"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".hero-date {"
  , "  letter-spacing: .2em;"
  , "  font-size: clamp(.7rem, 2.1vw, 1rem);"
  , "  color: rgba(255,255,255,.9);"
  , "  white-space: nowrap;"
  , "  margin-bottom: .45rem;"
  , "}"
  , ".hero-nav {"
  , "  display: flex;"
  , "  justify-content: center;"
  , "  flex-wrap: wrap;"
  , "  gap: .45rem 1rem;"
  , "  padding: .95rem 1rem 1.2rem;"
  , "  border-top: 1px solid rgba(255,255,255,.24);"
  , "  background: linear-gradient(180deg, rgba(20,12,6,.12) 0%, rgba(20,12,6,.42) 100%);"
  , "  backdrop-filter: blur(2px);"
  , "  position: relative;"
  , "  z-index: 2;"
  , "}"
  , ".hero-nav a {"
  , "  color: rgba(255,255,255,.84);"
  , "  text-decoration: none;"
  , "  font-size: .6rem;"
  , "  letter-spacing: .2em;"
  , "  text-transform: uppercase;"
  , "  transition: color .2s;"
  , "  display: inline-block;"  -- needed for magnetic transform
  , "}"
  , ".hero-nav a:hover { color: #fff; }"
  , "@media (max-width: 760px) {"
  , "  .hero-bg { background-size: auto 100%; }"
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
  , ""

  -- ── Glass cards ───────────────────────────────────────────────────────────
  , ".glass {"
  , "  background: rgba(138,108,76,.24);"
  , "  backdrop-filter: blur(22px) saturate(1.25);"
  , "  -webkit-backdrop-filter: blur(22px) saturate(1.25);"
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

  -- ── Dress code ────────────────────────────────────────────────────────────
  , ".dress-info {"
  , "  position: relative;"
  , "  z-index: 2;"
  , "  margin-top: 3rem;"
  , "}"
  , ".dress-runway {"
  , "  position: absolute;"
  , "  bottom: 4%;"
  , "  left: 0; right: 0;"
  , "  display: flex;"
  , "  justify-content: center;"
  , "  align-items: flex-end;"
  , "  gap: clamp(1rem, 6vw, 3rem);"
  , "  pointer-events: none;"
  , "  z-index: 2;"
  , "}"
  , ".dress-cutout {"
  , "  height: clamp(160px, 36vh, 320px);"
  , "  width: auto;"
  , "  filter: drop-shadow(0 12px 28px rgba(0,0,0,.55));"
  , "  will-change: transform, opacity;"
  , "}"
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
  , "  z-index: 200;"
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

  -- ── Countdown ─────────────────────────────────────────────────────────────
  , ".countdown {"
  , "  padding: 1.2rem 1.8rem 2.5rem;"
  , "  font-size: .68rem;"
  , "  letter-spacing: .2em;"
  , "  color: rgba(255,255,255,.52);"
  , "  text-transform: uppercase;"
  , "}"
  , ""

  -- ── Closing ───────────────────────────────────────────────────────────────
  , ".closing-text { padding: 0 1.8rem 1.8rem; }"
  , ".closing-line {"
  , "  display: block;"
  , "  font-family: 'Great Vibes', cursive;"
  , "  font-weight: 400;"
  , "  font-size: clamp(3.6rem, 15vw, 7.5rem);"
  , "  line-height: .9;"
  , "  color: #fff;"
  , "  overflow: visible;"
  , "}"
  , ""

  -- ── Back to top ───────────────────────────────────────────────────────────
  , ".back-to-top {"
  , "  position: fixed;"
  , "  bottom: 2rem;"
  , "  right: 1.6rem;"
  , "  z-index: 300;"
  , "  width: 3rem;"
  , "  height: 3rem;"
  , "  border-radius: 50%;"
  , "  background: rgba(138,108,76,.28);"
  , "  backdrop-filter: blur(18px) saturate(1.3);"
  , "  -webkit-backdrop-filter: blur(18px) saturate(1.3);"
  , "  border: 1px solid rgba(255,255,255,.22);"
  , "  cursor: pointer;"
  , "  padding: 0;"
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

  -- ── Magnetic elements ─────────────────────────────────────────────────────
  , "[data-magnetic] { display: inline-block; }"
  , ""

  -- ── Reduced motion ────────────────────────────────────────────────────────
  , "@media (prefers-reduced-motion: reduce) {"
  , "  .intro { display: none !important; }"
  , "  .progress-bar { display: none; }"
  , "  .sw-i { transform: none !important; }"
  , "  .intro-sign { opacity: 1; }"
  , "  [data-reveal] { opacity: 1; transform: none; }"
  , "  .zoom-section .section-photo { clip-path: none; transform: none; }"
  , "}"
  ]
