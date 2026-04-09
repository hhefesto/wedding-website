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
  closingSection

-- ── HERO ─────────────────────────────────────────────────────────────────────
-- Prototype 1:
--   • very light scrim over the photo
--   • "Daniel y" / "Ana Cristina" in Great Vibes, large, white, bottom-left
--   • date centered above the nav strip
--   • horizontal nav strip at the very bottom

heroSection :: DomBuilder t m => m ()
heroSection =
  sec "hero" "images/hero.jpg" $ do
    elAttr "div" ("class" =: "spacer") blank
    elAttr "div" ("class" =: "hero-names") $ do
      elAttr "span" ("class" =: "hero-name") $ text "Daniel y"
      elAttr "span" ("class" =: "hero-name") $ text "Ana Cristina"
    elAttr "p" ("class" =: "hero-date") $ text "10/10/26"
    elAttr "nav" ("class" =: "hero-nav") $ do
      navA "#ubicacion"    "UBICACIÓN"
      navA "#dress-code"   "DRESS CODE"
      navA "#rsvp"         "RSVP"
      navA "#mesa-regalos" "MESA DE REGALOS"

-- ── UBICACIÓN ────────────────────────────────────────────────────────────────
-- Prototype 2:
--   • "UBICACIÓN" small label at top-right
--   • large organic blob glass card, left-aligned, upper portion of screen
--   • couple photo fills the lower half (background shows through)

ubicacionSection :: DomBuilder t m => m ()
ubicacionSection =
  sec "ubicacion" "images/ubicacion.jpg" $ do
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
--   • dress-collage image below the card (optional — place at images/dress-collage.jpg)

dressCodeSection :: DomBuilder t m => m ()
dressCodeSection =
  sec "dress-code" "images/dress-code.jpg" $ do
    elAttr "p" ("class" =: "label label-center") $ text "DRESS CODE"
    elAttr "div" ("class" =: "glass rect") $ do
      el "p" $ text "Formal"
      el "p" $ text "H: traje y corbata"
      el "p" $ text "M: corto, midi, largo"
    elAttr "img"
      ( "class" =: "collage"
     <> "src"   =: "images/dress-collage.jpg"
     <> "alt"   =: ""
      ) blank
    elAttr "div" ("class" =: "spacer") blank

-- ── RSVP ─────────────────────────────────────────────────────────────────────

rsvpSection :: DomBuilder t m => m ()
rsvpSection =
  sec "rsvp" "images/rsvp.jpg" $ do
    elAttr "p" ("class" =: "label label-center") $ text "RSVP"
    elAttr "div" ("class" =: "glass rect") $ do
      el "p" $ text "Por favor confirma tu asistencia"
      el "p" $ text "antes del 10 de septiembre de 2026."
      elAttr "a"
        ( "class"  =: "rsvp-btn"
       <> "href"   =: "https://forms.gle/PLACEHOLDER"
       <> "target" =: "_blank"
       <> "rel"    =: "noopener noreferrer"
        ) $ text "Confirmar →"
    elAttr "div" ("class" =: "spacer") blank

-- ── MESA DE REGALOS ──────────────────────────────────────────────────────────
-- Prototype 4:
--   • glass card at top, label lives INSIDE the card
--   • photo fills the rest

mesaRegalosSection :: DomBuilder t m => m ()
mesaRegalosSection =
  sec "mesa-regalos" "images/mesa-regalos.jpg" $ do
    elAttr "div" ("class" =: "glass rect mesa") $ do
      elAttr "p" ("class" =: "mesa-label") $ text "MESA DE REGALOS"
      el "p" $ text "Liverpool  51981423"
    elAttr "div" ("class" =: "spacer") blank

-- ── CLOSING ──────────────────────────────────────────────────────────────────
-- Prototype 5:
--   • warm portrait, very light overlay
--   • "¡Nos vemos el" / "10 de octubre!" in Great Vibes, large, bottom-left

closingSection :: DomBuilder t m => m ()
closingSection =
  sec "closing" "images/closing.jpg" $ do
    elAttr "div" ("class" =: "spacer") blank
    elAttr "div" ("class" =: "closing-text") $ do
      elAttr "span" ("class" =: "closing-line") $ text "\xa1Nos vemos el"
      elAttr "span" ("class" =: "closing-line") $ text "10 de octubre!"

-- ── Helpers ───────────────────────────────────────────────────────────────────

sec :: DomBuilder t m => Text -> Text -> m () -> m ()
sec sid img =
  elAttr "section"
    ( "id"    =: sid
   <> "class" =: "section"
   <> "style" =: ("background-image:url(" <> img <> ")")
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
  , "  background-size: cover;"
  , "  background-position: center;"
  , "  background-repeat: no-repeat;"
  , "  overflow: hidden;"
  , "  isolation: isolate;"     -- stacking context: ::before z-index:-1 stays inside
  , "}"
  , ""
  -- Warm scrim — lives below all children (z-index:-1 within the isolated context)
  , ".section::before {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  background: rgba(18,12,5,.20);"
  , "  z-index: -1;"
  , "  pointer-events: none;"
  , "}"
  , ""
  -- Dress-code: much stronger amber overlay, matching prototype 3
  , "#dress-code::before { background: rgba(52,28,4,.62); }"
  , ""
  -- Fallback colours shown when background images are not yet placed
  , "#hero         { background-color: #3d2e22; }"
  , "#ubicacion    { background-color: #2e3a28; }"
  , "#dress-code   { background-color: #4a3010; }"
  , "#rsvp         { background-color: #2a3035; }"
  , "#mesa-regalos { background-color: #382e24; }"
  , "#closing      { background-color: #4a3220; }"
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
  -- Blob: matches prototype 2 — large organic oval covering ~75 % of width
  , ".blob {"
  , "  border-radius: 44% 56% 38% 62% / 52% 44% 56% 48%;"
  , "  width: calc(100% - 3.6rem);"
  , "  max-width: 420px;"
  , "}"
  , ""
  -- Rect: rounded rectangle for dress-code, rsvp (prototype 3 card shape)
  , ".rect { border-radius: 14px; max-width: 320px; }"
  , ""
  -- Mesa: same rect but label lives inside (prototype 4)
  , ".mesa { max-width: 340px; line-height: 1.7; }"
  , ".mesa-label {"
  , "  font-size: .63rem;"
  , "  letter-spacing: .27em;"
  , "  text-transform: uppercase;"
  , "  color: rgba(255,255,255,.87);"
  , "  margin-bottom: .9rem;"
  , "}"
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
  -- Hide broken-image icon when file not yet placed
  , ".collage[src='images/dress-collage.jpg'] { min-height: 0; }"
  , ""

  -- ── RSVP button ───────────────────────────────────────────────────────────
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
  , "  transition: background .2s;"
  , "}"
  , ".rsvp-btn:hover { background: rgba(255,255,255,.12); }"
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
