{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

-- ── Entry point ───────────────────────────────────────────────────────────────

main :: IO ()
main = mainWidgetWithHead headW bodyW

-- Everything in <head> comes from Haskell
headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Daniel y Ana Cristina — 10 · 10 · 26"
  el "style" $ text siteCSS

-- ── Sections ──────────────────────────────────────────────────────────────────

bodyW :: DomBuilder t m => m ()
bodyW = do
  heroSection
  ubicacionSection
  dressCodeSection
  rsvpSection
  mesaRegalosSection
  videoMsgSection
  closingSection
  rsvpOverlay
  backToTop

-- ── Back to top ──────────────────────────────────────────────────────────────
-- Floating glass circle with upward arrow, shown after scrolling past hero.

backToTop :: DomBuilder t m => m ()
backToTop =
  elAttr "button"
    ( "id"         =: "back-to-top"
   <> "class"      =: "back-to-top"
   <> "aria-label" =: "Volver arriba"
   <> "style"      =: "display:none"
    ) blank
  -- Arrow is drawn via CSS ::before (two borders + rotate)

-- ── HERO ─────────────────────────────────────────────────────────────────────
-- Prototype 1:
--   • very light scrim over the photo
--   • "Daniel y" / "Ana Cristina" in Great Vibes, large, white, bottom-left
--   • date centered above the nav strip
--   • horizontal nav strip at the very bottom

heroSection :: DomBuilder t m => m ()
heroSection =
  sec "hero" $ do
    elAttr "div" ("class" =: "spacer") blank
    elAttr "div" ("class" =: "hero-names") $ do
      elAttr "span" ("class" =: "hero-name") $ text "Daniel y"
      elAttr "span" ("class" =: "hero-name") $ text "Ana Cristina"
    elAttr "p" ("class" =: "hero-date") $ text "10/10/26"
    elAttr "nav" ("class" =: "hero-nav") $ do
      navA "#ubicacion"     "UBICACIÓN"
      navA "#dress-code"    "DRESS CODE"
      navA "#rsvp"          "RSVP"
      navA "#mesa-regalos"  "MESA DE REGALOS"
      navA "#video-mensaje" "VIDEO"

-- ── UBICACIÓN ────────────────────────────────────────────────────────────────
-- Prototype 2:
--   • "UBICACIÓN" small label at top-right
--   • large organic blob glass card, left-aligned, upper portion of screen
--   • couple photo fills the lower half (background shows through)

ubicacionSection :: DomBuilder t m => m ()
ubicacionSection =
  sec "ubicacion" $ do
    elAttr "p" ("class" =: "label label-right") $ text "UBICACIÓN"
    elAttr "div" ("class" =: "glass blob") $ do
      el "p" $ text "Gran Terraza"
      el "p" $ text "Vista Real Country Club"
      el "p" $ text "6 pm"
    elAttr "div" ("class" =: "spacer") blank

-- ── DRESS CODE ───────────────────────────────────────────────────────────────
-- Prototype 3:
--   • STRONG warm brown overlay (much darker than other sections)
--   • "DRESS CODE" label top-center
--   • rounded-rect glass card with dress info
--   • dress-collage image below the card (optional)

dressCodeSection :: DomBuilder t m => m ()
dressCodeSection =
  sec "dress-code" $ do
    elAttr "p" ("class" =: "label label-center") $ text "DRESS CODE"
    elAttr "div" ("class" =: "glass rect") $ do
      el "p" $ text "Formal"
      el "p" $ text "H: traje y corbata"
      el "p" $ text "M: corto, midi, largo"
    elAttr "img"
      ( "class"   =: "collage"
     <> "src"     =: "images/dress-collage.png"
     <> "alt"     =: ""
     <> "onerror" =: "this.style.display='none'"
      ) blank
    elAttr "div" ("class" =: "spacer") blank

-- ── RSVP ─────────────────────────────────────────────────────────────────────

rsvpSection :: DomBuilder t m => m ()
rsvpSection =
  sec "rsvp" $ do
    elAttr "p" ("class" =: "label label-center") $ text "RSVP"
    elAttr "div" ("class" =: "glass rect") $ do
      el "p" $ text "Por favor confirma tu asistencia"
      el "p" $ text "antes del 10 de septiembre de 2026."
      elAttr "button"
        ( "class" =: "rsvp-btn"
       <> "id"    =: "rsvp-open-btn"
        ) $ text "Confirmar \8594"
    elAttr "div" ("class" =: "spacer") blank

-- RSVP multi-step WhatsApp overlay (fixed position, outside sections)
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
-- Prototype 4:
--   • multiple registry cards stacked, each with copy-to-clipboard

mesaRegalosSection :: DomBuilder t m => m ()
mesaRegalosSection =
  sec "mesa-regalos" $ do
    elAttr "div" ("class" =: "registries") $ do
      registryCard "LIVERPOOL" "51981423" "51981423"
      registryCard "PR\211XIMAMENTE" "\8212" ""
    elAttr "div" ("class" =: "spacer") blank

registryCard :: DomBuilder t m => Text -> Text -> Text -> m ()
registryCard name number copyVal =
  elAttr "div" ("class" =: "glass rect registry-card") $ do
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
  sec "video-mensaje" $ do
    elAttr "p" ("class" =: "label label-center") $
      text "VIDEO PARA LOS NOVIOS"
    elAttr "div" ("class" =: "glass rect video-card") $ do
      elAttr "span" ("class" =: "video-msg-icon") $ text "\127916"
      elAttr "p" ("class" =: "video-msg-text") $
        text "Gr\225banos un video corto deseando lo mejor a los novios \
             \y env\237anoslo por WhatsApp."
      elAttr "a"
        ( "class"  =: "rsvp-btn video-wa-btn"
       <> "href"   =: "https://wa.me/PLACEHOLDER?text=\
                       \Video%20para%20Daniel%20y%20Ana%20Cristina%20\
                       \%F0%9F%8E%AC"
       <> "target" =: "_blank"
       <> "rel"    =: "noopener noreferrer"
        ) $ text "Enviar video \8594"
    elAttr "div" ("class" =: "spacer") blank

-- ── CLOSING ──────────────────────────────────────────────────────────────────
-- Prototype 5:
--   • warm portrait, very light overlay
--   • "¡Nos vemos el" / "10 de octubre!" in Great Vibes, large, bottom-left

closingSection :: DomBuilder t m => m ()
closingSection =
  sec "closing" $ do
    elAttr "div" ("class" =: "spacer") blank
    elAttr "div" ("class" =: "closing-text") $ do
      elAttr "span" ("class" =: "closing-line") $ text "\xa1Nos vemos el"
      elAttr "span" ("class" =: "closing-line") $ text "10 de octubre!"

-- ── Helpers ───────────────────────────────────────────────────────────────────

sec :: DomBuilder t m => Text -> m () -> m ()
sec sid =
  elAttr "section"
    ( "id"    =: sid
   <> "class" =: "section"
    )

navA :: DomBuilder t m => Text -> Text -> m ()
navA href label = elAttr "a" ("href" =: href) $ text label

-- ── All CSS ───────────────────────────────────────────────────────────────────
-- Google Fonts @import must come first in the stylesheet.

siteCSS :: Text
siteCSS = T.unlines

  -- Fonts (loaded here so index.html stays a bare skeleton)
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
  , "  background-size: auto 100%;"
  , "  background-position: center;"
  , "  background-repeat: no-repeat;"
  , "  overflow: hidden;"
  , "  isolation: isolate;"
  , "}"
  , ""
  -- Warm scrim
  , ".section::before {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  background: rgba(18,12,5,.20);"
  , "  z-index: -1;"
  , "  pointer-events: none;"
  , "}"
  , ""
  -- Dress-code: stronger amber overlay
  , "#dress-code::before { background: rgba(52,28,4,.62); }"
  , ""
  -- Background images
  , "#hero         { background-image: url('images/hero.png');         background-color: #3d2e22; }"
  , "#ubicacion    { background-image: url('images/ubicacion.png');    background-color: #2e3a28; }"
  , "#dress-code   { background-image: url('images/dress-code.png');   background-color: #4a3010; }"
  , "#rsvp         { background-image: url('images/rsvp.png');         background-color: #2a3035; }"
  , "#mesa-regalos { background-image: url('images/mesa-regalos.png'); background-color: #382e24; }"
  , "#video-mensaje { background-color: #2c2418; }"
  , "#closing      { background-image: url('images/closing.png');      background-color: #4a3220; }"
  , ""
  -- Flex spacer
  , ".spacer { flex: 1; }"
  , ""

  -- ── Hero ──────────────────────────────────────────────────────────────────
  , ".hero-names { padding: 0 1.8rem .12rem; }"
  , ".hero-name {"
  , "  display: block;"
  , "  font-family: 'Great Vibes', cursive;"
  , "  font-weight: 400;"
  , "  font-size: clamp(4.4rem, 18vw, 8.4rem);"
  , "  line-height: .88;"
  , "  color: #fff;"
  , "}"
  , ".hero-date {"
  , "  text-align: center;"
  , "  letter-spacing: .28em;"
  , "  font-size: .68rem;"
  , "  color: rgba(255,255,255,.82);"
  , "  padding: 1rem 0 .78rem;"
  , "}"
  , ".hero-nav {"
  , "  display: flex;"
  , "  justify-content: space-around;"
  , "  flex-wrap: wrap;"
  , "  gap: .3rem 0;"
  , "  padding: .85rem .5rem;"
  , "  border-top: 1px solid rgba(255,255,255,.17);"
  , "}"
  , ".hero-nav a {"
  , "  color: rgba(255,255,255,.76);"
  , "  text-decoration: none;"
  , "  font-size: .58rem;"
  , "  letter-spacing: .22em;"
  , "  text-transform: uppercase;"
  , "  transition: color .2s;"
  , "}"
  , ".hero-nav a:hover { color: #fff; }"
  , ""

  -- ── Section labels ────────────────────────────────────────────────────────
  , ".label {"
  , "  font-size: .63rem;"
  , "  letter-spacing: .27em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.87);"
  , "  padding: 1.8rem 1.8rem 0;"
  , "}"
  , ".label-right  { text-align: right; }"
  , ".label-center { text-align: center; }"
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
  , "}"
  , ""
  , ".blob {"
  , "  border-radius: 44% 56% 38% 62% / 52% 44% 56% 48%;"
  , "  width: calc(100% - 3.6rem);"
  , "  max-width: 420px;"
  , "}"
  , ""
  , ".rect { border-radius: 14px; max-width: 320px; }"
  , ""

  -- ── Dress-code collage ────────────────────────────────────────────────────
  , ".collage {"
  , "  display: block;"
  , "  width: calc(100% - 3.6rem);"
  , "  max-width: 420px;"
  , "  margin: .6rem 1.8rem 0;"
  , "  border-radius: 10px;"
  , "  object-fit: contain;"
  , "  max-height: 52vh;"
  , "}"
  , ".collage[src='images/dress-collage.jpg'] { min-height: 0; }"
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

  -- ── Mesa de Regalos ───────────────────────────────────────────────────────
  , ".registries {"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  gap: .8rem;"
  , "  margin: 1.1rem 1.8rem 0;"
  , "}"
  , ".registry-card {"
  , "  max-width: 340px;"
  , "  line-height: 1.7;"
  , "  margin: 0;"
  , "}"
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
  , ""

  -- ── Video mensaje ─────────────────────────────────────────────────────────
  , ".video-card { max-width: 320px; text-align: center; }"
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
  -- Arrow: two thin lines forming a chevron ∧
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

  -- ── Closing ───────────────────────────────────────────────────────────────
  , ".closing-text { padding: 0 1.8rem 4rem; }"
  , ".closing-line {"
  , "  display: block;"
  , "  font-family: 'Great Vibes', cursive;"
  , "  font-weight: 400;"
  , "  font-size: clamp(3.6rem, 15vw, 7.5rem);"
  , "  line-height: .9;"
  , "  color: #fff;"
  , "}"
  ]
