{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex.Dom

main :: IO ()
main = mainWidget site

-- ── Top-level layout ──────────────────────────────────────────────────────────

site :: DomBuilder t m => m ()
site = do
  heroSection
  ubicacionSection
  dressCodeSection
  rsvpSection
  mesaRegalosSection
  closingSection

-- ── Hero ──────────────────────────────────────────────────────────────────────
-- Prototype 1: full-bleed photo, script names bottom-left, date, bottom nav

heroSection :: DomBuilder t m => m ()
heroSection =
  elAttr "section" (sectionAttrs "hero" "images/hero.jpg") $ do
    elAttr "div" ("class" =: "hero__body") $ do
      elAttr "h1" ("class" =: "script hero__names") $ text "Daniel y"
      elAttr "h1" ("class" =: "script hero__names") $ text "Ana Cristina"
    elAttr "p" ("class" =: "hero__date") $ text "10 / 10 / 26"
    elAttr "nav" ("class" =: "hero__nav") $ do
      navLink "#ubicacion"    "UBICACIÓN"
      navLink "#dress-code"   "DRESS CODE"
      navLink "#rsvp"         "RSVP"
      navLink "#mesa-regalos" "MESA DE REGALOS"

-- ── Ubicación ─────────────────────────────────────────────────────────────────
-- Prototype 2: full photo, label top-right, frosted card with venue info

ubicacionSection :: DomBuilder t m => m ()
ubicacionSection =
  elAttr "section" (sectionAttrs "ubicacion" "images/ubicacion.jpg") $ do
    elAttr "div" ("class" =: "section__label section__label--right") $
      text "UBICACIÓN"
    elAttr "div" ("class" =: "glass-card") $ do
      el "p" $ text "Gran Terraza"
      el "p" $ text "Vista Real Country Club"
      el "p" $ text "6 pm"

-- ── Dress Code ────────────────────────────────────────────────────────────────
-- Prototype 3: forest photo, label top-center, frosted card, dress collage

dressCodeSection :: DomBuilder t m => m ()
dressCodeSection =
  elAttr "section" (sectionAttrs "dress-code" "images/dress-code.jpg") $ do
    elAttr "div" ("class" =: "section__label section__label--center") $
      text "DRESS CODE"
    elAttr "div" ("class" =: "glass-card") $ do
      el "p" $ text "Formal"
      el "p" $ text "H: traje y corbata"
      el "p" $ text "M: corto, midi, largo"
    -- Optional dress-collage image (place at public/images/dress-collage.jpg)
    elAttr "img"
      ( "class" =: "dress-collage"
     <> "src"   =: "images/dress-collage.jpg"
     <> "alt"   =: "Dress code examples"
      ) blank

-- ── RSVP ──────────────────────────────────────────────────────────────────────

rsvpSection :: DomBuilder t m => m ()
rsvpSection =
  elAttr "section" (sectionAttrs "rsvp" "images/rsvp.jpg") $ do
    elAttr "div" ("class" =: "section__label section__label--center") $
      text "RSVP"
    elAttr "div" ("class" =: "glass-card glass-card--rsvp") $ do
      elAttr "p" ("class" =: "rsvp__text") $
        text "Por favor confirma tu asistencia antes del 10 de septiembre de 2026."
      elAttr "a"
        ( "href"  =: "https://forms.gle/PLACEHOLDER"
       <> "class" =: "rsvp__btn"
       <> "target" =: "_blank"
       <> "rel"  =: "noopener noreferrer"
        ) $ text "Confirmar asistencia →"

-- ── Mesa de Regalos ───────────────────────────────────────────────────────────
-- Prototype 4: outdoor photo, frosted card with registry info

mesaRegalosSection :: DomBuilder t m => m ()
mesaRegalosSection =
  elAttr "section" (sectionAttrs "mesa-regalos" "images/mesa-regalos.jpg") $ do
    elAttr "div" ("class" =: "section__label section__label--right") $
      text "MESA DE REGALOS"
    elAttr "div" ("class" =: "glass-card") $ do
      el "p" $ text "Liverpool"
      el "p" $ text "No. 51981423"

-- ── Closing ───────────────────────────────────────────────────────────────────
-- Prototype 5: warm portrait photo, script closing message bottom-left

closingSection :: DomBuilder t m => m ()
closingSection =
  elAttr "section" (sectionAttrs "closing" "images/closing.jpg") $
    elAttr "div" ("class" =: "closing__body") $ do
      elAttr "h2" ("class" =: "script closing__msg") $ text "¡Nos vemos el"
      elAttr "h2" ("class" =: "script closing__msg") $ text "10 de octubre!"

-- ── Helpers ───────────────────────────────────────────────────────────────────

sectionAttrs :: Text -> Text -> Map Text Text
sectionAttrs sectionId bgImg =
     "id"    =: sectionId
  <> "class" =: "section"
  <> "style" =: ("background-image: url('" <> bgImg <> "')")

navLink :: DomBuilder t m => Text -> Text -> m ()
navLink href label =
  elAttr "a" ("href" =: href <> "class" =: "hero__nav-link") $ text label
