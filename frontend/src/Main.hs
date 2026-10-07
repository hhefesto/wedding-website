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
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.Window as Window
import Wedding.Types (AttendanceStatus (..), RsvpRequest (..))

import Fotos (fotosCSS, fotosPage, galleryBand, galleryFeed)

-- ── Entry point ───────────────────────────────────────────────────────────────

main :: IO ()
main = mainWidgetWithHead headW bodyW

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Daniel y Ana Cristina — 10 · 10 · 26"
  el "style" $ text (siteCSS <> fotosCSS)

-- ── Body ──────────────────────────────────────────────────────────────────────

-- /fotos is the focused upload page the QR code points to; everything else
-- is the invitation.
bodyW :: (MonadWidget t m, MonadJSM (Performable m)) => m ()
bodyW = do
  path <- liftJSM $ DOM.currentWindowUnchecked >>= Window.getLocation >>= Location.getPathname
  if T.dropWhileEnd (== '/') path == "/fotos"
    then fotosPage
    else siteW

siteW :: (MonadWidget t m, MonadJSM (Performable m)) => m ()
siteW = do
  elAttr "div" ("class" =: "site-shell") $ do
    introOverlay
    progressBar
    heroSection
    itinerarioSection
    rsvpSection
    ubicacionSection
    dressCodeSection
    mesaRegalosSection
    fotosSection
    galleryBand "galeria" =<< galleryFeed never
    fixedNav
    backToTop
  pb <- getPostBuild
  performEvent_ $ liftJSM (void $ eval (navHighlightingJS <> ";" <> settleOnViewJS <> ";" <> cardScrollIndicatorsJS <> ";" <> rsvpInlinePrefillJS)) <$ pb

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
  <> "if(lnk){lnk.classList.toggle('is-active',e.isIntersecting);"
  <> "var nav=lnk.parentNode;if(e.isIntersecting&&nav.scrollWidth>nav.clientWidth){nav.scrollTo({left:lnk.offsetLeft-(nav.clientWidth-lnk.offsetWidth)/2,behavior:'smooth'});}}"
  <> "});"
  <> "},{rootMargin:'-40% 0px -40% 0px',threshold:0});"
  -- postBuild fires before the widget tree is attached, so wait for it.
  <> "(function start(){var secs=document.querySelectorAll('.image-section');"
  <> "if(!secs.length){setTimeout(start,50);return;}"
  <> "secs.forEach(function(s){obs.observe(s);});})();"
  <> "})()"

cardScrollIndicatorsJS :: String
cardScrollIndicatorsJS =
  "(function(){"
  <> "function cards(){return Array.prototype.slice.call(document.querySelectorAll('.section-overlay .glass'));}"
  <> "function ensure(card){var ind=card.__weddingScrollIndicator;if(ind)return ind;var overlay=card.closest('.section-overlay');if(!overlay)return null;ind=document.createElement('div');ind.className='card-scroll-indicator';ind.setAttribute('aria-hidden','true');var thumb=document.createElement('div');thumb.className='card-scroll-indicator-thumb';ind.appendChild(thumb);overlay.appendChild(ind);card.__weddingScrollIndicator=ind;card.addEventListener('scroll',function(){update(card);},{passive:true});return ind;}"
  <> "function update(card){var ind=ensure(card);if(!ind)return;var overflow=card.scrollHeight-card.clientHeight>1;card.classList.toggle('has-card-scroll',overflow);ind.classList.toggle('is-visible',overflow);if(!overflow)return;var overlay=ind.parentNode;var r=card.getBoundingClientRect();var o=overlay.getBoundingClientRect();var inset=10;var trackH=Math.max(34,r.height-inset*2);var maxScroll=Math.max(1,card.scrollHeight-card.clientHeight);var thumbH=Math.max(32,trackH*(card.clientHeight/card.scrollHeight));var maxTop=Math.max(0,trackH-thumbH);var thumbTop=(card.scrollTop/maxScroll)*maxTop;ind.style.left=(r.right-o.left-inset)+'px';ind.style.top=(r.top-o.top+inset)+'px';ind.style.height=trackH+'px';ind.firstChild.style.height=thumbH+'px';ind.firstChild.style.transform='translateY('+thumbTop+'px)';}"
  <> "function updateAll(){cards().forEach(update);}"
  <> "function schedule(){requestAnimationFrame(updateAll);}"
  <> "window.addEventListener('resize',schedule,{passive:true});window.addEventListener('orientationchange',schedule,{passive:true});window.addEventListener('load',schedule,{passive:true});"
  <> "if(window.ResizeObserver){var ro=new ResizeObserver(schedule);cards().forEach(function(c){ro.observe(c);});}"
  <> "if(window.MutationObserver){new MutationObserver(schedule).observe(document.body,{childList:true,subtree:true,characterData:true});}"
  <> "schedule();var n=0,t=setInterval(function(){updateAll();if(++n>80)clearInterval(t);},100);"
  <> "})()"

-- Plays [data-settle] entrances once, the first time each element is seen.
-- Elements are only hidden after this runs, so without JS they stay visible.
settleOnViewJS :: String
settleOnViewJS =
  "(function(){if(!window.IntersectionObserver)return;"
  <> "var io=new IntersectionObserver(function(es){es.forEach(function(e){"
  <> "if(e.isIntersecting){e.target.classList.add('is-in');io.unobserve(e.target);}});},{threshold:.2});"
  <> "(function start(){var xs=document.querySelectorAll('[data-settle]');"
  <> "if(!xs.length){setTimeout(start,50);return;}"
  <> "xs.forEach(function(x){x.classList.add('is-armed');io.observe(x);});})();"
  <> "})()"

rsvpInlinePrefillJS :: String
rsvpInlinePrefillJS =
  "(function(){function set(id,v){var el=document.getElementById(id);if(el&&el.value!==v){el.value=v;el.dispatchEvent(new Event('input',{bubbles:true}));}}function start(){var c=new URLSearchParams(location.search||'').get('code')||'';if(!c){set('rsvp-invitation-code','');return;}if(window.__weddingRsvpInlinePrefill)return;window.__weddingRsvpInlinePrefill=1;fetch('/api/invite?code='+encodeURIComponent(c),{credentials:'same-origin'}).then(function(r){if(!r.ok)throw new Error(String(r.status));return r.json();}).then(function(i){set('rsvp-invitation-code',c);set('rsvp-name',i.name||'');}).catch(function(){set('rsvp-invitation-code','');});}start();var n=0,t=setInterval(function(){start();if(++n>100)clearInterval(t);},50);})()"

fixedNav :: DomBuilder t m => m ()
fixedNav =
  elAttr "nav"
    ( "id"         =: "fixed-nav"
   <> "class"      =: "fixed-nav"
   <> "aria-label" =: "Secciones"
    ) $
    elAttr "div" ("class" =: "fixed-nav-track") $
    forM_ navItems $ \(href, label) ->
      elAttr "a"
        ( "href"         =: href
       <> "class"        =: "fixed-nav-link"
       <> "data-section" =: T.drop 1 href
        ) $ text label
  where
    navItems :: [(Text, Text)]
    navItems =
      [ ("#itinerario",    "ITINERARIO")
      , ("#rsvp",          "RSVP")
      , ("#ubicacion",     "UBICACI\211N")
      , ("#dress-code",    "DRESS CODE")
      , ("#mesa-regalos",  "REGALOS")
      , ("#fotos",         "FOTOS")
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
        qrBlock True "https://www.google.com/maps/search/?api=1&query=20.5229282,-100.4039031" "qr-location.png" "Abrir ubicaci\243n"
        elAttr "iframe"
          ( "class"          =: "map-embed"
         <> "src"            =: "https://maps.google.com/maps?q=20.5229282,-100.4039031&z=17&output=embed&hl=es"
         <> "allowfullscreen" =: ""
         <> "loading"        =: "lazy"
         <> "referrerpolicy" =: "no-referrer-when-downgrade"
          ) blank

-- ── ITINERARIO ───────────────────────────────────────────────────────────────
-- The printed itinerary card, presented as stationery resting on a dark,
-- candle-lit table. The image already carries its own typography, so there is
-- no glass card: the paper itself is the content. Tapping opens it full size.

itinerarioSection :: DomBuilder t m => m ()
itinerarioSection =
  secImage "itinerario" $
    elAttr "div" ("class" =: "itinerario-stage") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "ITINERARIO"
      elAttr "a"
        ( "class"  =: "itinerario-link"
       <> "href"   =: "images/itinerario.jpeg"
       <> "target" =: "_blank"
       <> "rel"    =: "noopener"
        ) $ do
        elAttr "span" ("class" =: "itinerario-settle" <> "data-settle" =: "") $
          elAttr "span" ("class" =: "itinerario-card") $ do
            elAttr "img"
              ( "class"    =: "itinerario-img"
             <> "src"      =: "images/itinerario.jpeg"
             <> "alt"      =: "Itinerario del 10 de octubre de 2026: 5:30 p. m. llegada de invitados; 6:00 p. m. ceremonia; 7:00 p. m. c\243ctel; 8:00 p. m. cena."
             <> "width"    =: "1070"
             <> "height"   =: "1470"
             <> "loading"  =: "lazy"
             <> "decoding" =: "async"
              ) blank
            elAttr "span" ("class" =: "itinerario-sheen" <> "aria-hidden" =: "true") blank
        elAttr "span" ("class" =: "itinerario-hint") $ text "VER EN GRANDE"

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

rsvpSection :: MonadWidget t m => m ()
rsvpSection =
  secImage "rsvp" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "./images/6.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $ text "R\233pondez s'il vous pla\238t"
      elAttr "div" ("class" =: "glass rect rsvp-confirm rsvp-inline" <> "data-reveal" =: "") $ mdo
        el "p" $ text "Por favor responde si podr\225s acompa\241arnos"
        el "p" $ text "antes del 10 de septiembre de 2026."
        elAttr "p" ("class" =: "rsvp-adults-note") $ text "Celebraci\243n solo para adultos. Cada RSVP permite hasta 2 adultos."
        nameEl <- inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("id" =: "rsvp-name" <> "class" =: "rsvp-input" <> "placeholder" =: "Tu nombre" <> "required" =: "required")
        codeEl <- inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("id" =: "rsvp-invitation-code" <> "type" =: "hidden")
        declineEl <- elAttr "label" ("class" =: "rsvp-check") $ do
          el <- inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox" <> "id" =: "rsvp-decline")
          text " No podremos asistir"
          pure el
        let declinedD = _inputElement_checked declineEl
        dyn_ $ ffor declinedD $ \declined ->
          if declined
            then elAttr "p" ("class" =: "rsvp-step-label") $ text "Registraremos tu respuesta con 0 adultos."
            else blank
        dyn_ $ ffor declinedD $ \declined ->
          if declined then blank else elAttr "p" ("class" =: "rsvp-step-label") $ text "\191Cu\225ntos adultos asistir\225n?"
        countDyn <- foldDyn ($) (1 :: Int) $ leftmost
          [ (\n -> max 1 (n - 1)) <$ minusE
          , (\n -> min 2 (n + 1)) <$ plusE
          ]
        (minusE, plusE) <- elDynAttr "div"
          (ffor declinedD $ \declined -> "class" =: "rsvp-counter" <> if declined then "style" =: "display:none" else mempty) $ do
          (minEl, _) <- elAttr' "button" ("class" =: "rsvp-counter-btn" <> "type" =: "button") $ text "\8722"
          el "span" $ dynText (T.pack . show <$> countDyn)
          (plusEl, _) <- elAttr' "button" ("class" =: "rsvp-counter-btn" <> "type" =: "button") $ text "+"
          pure (domEvent Click minEl, domEvent Click plusEl)
        dietaryEl <- inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: "text" <> "class" =: "rsvp-input" <> "placeholder" =: "Restricciones alimentarias (opcional)")
        let statusD = ffor declinedD $ \d -> if d then Declined else Attending
            guestsD = zipDynWith (\d n -> if d then 0 else n) declinedD countDyn
            rsvpDyn = RsvpRequest <$> (T.strip <$> _inputElement_value nameEl) <*> (T.strip <$> _inputElement_value codeEl) <*> statusD <*> guestsD <*> _inputElement_value dietaryEl <*> pure []
            reqDyn = ffor rsvpDyn $ \r -> XhrRequest "POST" "/api/rsvp" $ def
              & xhrRequestConfig_headers .~ ("Content-Type" =: "application/json")
              & xhrRequestConfig_sendData .~ TE.decodeUtf8 (BL.toStrict (encode r))
        (sendBtnEl, _) <- elAttr' "button" ("class" =: "rsvp-btn rsvp-send-btn" <> "type" =: "button") $ text "Enviar confirmaci\243n \8594"
        respE <- performRequestAsync (current reqDyn `tag` domEvent Click sendBtnEl)
        let resultE = ffor respE $ \resp -> if xhrSuccess resp then StatusSuccess else StatusError (xhrErrorText resp)
        statusDyn <- holdDyn StatusIdle $ leftmost [StatusSending <$ domEvent Click sendBtnEl, resultE]
        elDynAttr "p"
          (ffor statusDyn $ \s -> "class" =: "rsvp-status" <> if statusVisible s then mempty else "style" =: "display:none")
          $ dynText (statusMsg <$> statusDyn)
        pure ()

-- ── RSVP overlay — invitation-code response flow ─────────────────────────────

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

    (n1E, n2E, n3E, codeD, statusD, guestD, dietaryD) <-
      elAttr "div" ("class" =: "rsvp-modal") $ do

        -- Step 1: invitation code
        (codeD', n1E') <- rsvpStep stepDyn 1 $ do
          elAttr "p" ("class" =: "rsvp-step-label") $ text "Ingresa tu c\243digo de invitaci\243n"
          ti <- inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
               (  "type"        =: "text"
              <> "id"          =: "rsvp-invitation-code"
              <> "class"       =: "rsvp-input"
              <> "placeholder" =: "Ej. FAMILIA123"
              <> "autocomplete" =: "off"
              )
          (nb, _) <- elAttr' "button" ("class" =: "rsvp-btn" <> "type" =: "button") $ text "Continuar \8594"
          return (_inputElement_value ti, domEvent Click nb)

        -- Step 2: attendance response
        (statusD', n2E') <- rsvpStep stepDyn 2 $ do
          elAttr "p" ("class" =: "rsvp-step-label") $ text "\191Podr\225s acompa\241arnos?"
          yesE <- rsvpChoiceButton "S\237, asistir\233"
          noE  <- rsvpChoiceButton "No podr\233 asistir"
          statusD'' <- holdDyn Attending $ leftmost [Attending <$ yesE, Declined <$ noE, Attending <$ openE]
          let nextChoiceE = leftmost [yesE, noE]
          return (statusD'', nextChoiceE)

        -- Step 3: guest count and dietary restrictions
        (guestD', dietaryD', n3E') <- rsvpStep stepDyn 3 $ mdo
          dyn_ $ ffor statusD' $ \status ->
            if status == Declined
              then elAttr "p" ("class" =: "rsvp-step-label") $ text "Gracias por avisarnos. Env\237a tu respuesta para registrarla."
              else elAttr "p" ("class" =: "rsvp-step-label") $ text "\191Cu\225ntos asistir\225n?"
          countDyn <- foldDyn ($) (1 :: Int) $ leftmost
            [ (\n -> max 1  (n - 1)) <$ minusE
            , (\n -> min 20 (n + 1)) <$ plusE
            , const 1               <$ openE
            ]
          (minusE, plusE) <- elDynAttr "div"
            (ffor statusD' $ \status -> "class" =: "rsvp-counter" <> if status == Declined then "style" =: "display:none" else mempty) $ do
            (minEl, _) <- elAttr' "button" ("class" =: "rsvp-counter-btn") $ text "\8722"
            el "span" $ dynText (T.pack . show <$> countDyn)
            (plusEl, _) <- elAttr' "button" ("class" =: "rsvp-counter-btn") $ text "+"
            return (domEvent Click minEl, domEvent Click plusEl)
          ti <- inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              (  "type"        =: "text"
              <> "class"       =: "rsvp-input"
              <> "placeholder" =: "Restricciones alimentarias (opcional)"
              )
          (nb, _) <- elAttr' "button" ("class" =: "rsvp-btn" <> "type" =: "button") $ text "Continuar \8594"
          let guestsD = zipDynWith (\status guests -> if status == Declined then 0 else guests) statusD' countDyn
          return (guestsD, _inputElement_value ti, domEvent Click nb)

        -- Step 4: summary + POST submission
        rsvpStep_ stepDyn 4 $ mdo
          elAttr "p" ("class" =: "rsvp-step-label") $ text "\161Todo listo!"
          let summaryDyn = summaryRows <$> codeD' <*> statusD' <*> guestD' <*> dietaryD'
          elAttr "div" ("class" =: "rsvp-summary") $
            dyn_ $ ffor summaryDyn $ \rows ->
              forM_ rows $ \row -> el "p" $ text row

          let rsvpDyn = RsvpRequest <$> pure "" <*> (T.strip <$> codeD') <*> statusD' <*> guestD' <*> dietaryD' <*> pure []
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
                  _                        -> StatusError (xhrErrorText resp)
          statusDyn <- holdDyn StatusIdle $ leftmost
            [ StatusSending <$ sendE
            , resultE
            ]

          elDynAttr "p"
            ( ffor statusDyn $ \s ->
                "class" =: "rsvp-status"
             <> if statusVisible s then mempty else "style" =: "display:none"
            ) $ dynText (statusMsg <$> statusDyn)

        return (n1E', n2E', n3E', codeD', statusD', guestD', dietaryD')

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
summaryRows :: Text -> AttendanceStatus -> Int -> Text -> [Text]
summaryRows code status guests dietary =
  [ "C\243digo: " <> if T.null (T.strip code) then "\8212" else T.strip code
  , "Respuesta: " <> case status of
      Attending -> "S\237 asistir\233"
      Declined  -> "No podr\233 asistir"
  , "Asistentes: " <> T.pack (show guests)
  ] ++ [ "Restricciones: " <> dietary | status == Attending && not (T.null dietary) ]

rsvpChoiceButton :: DomBuilder t m => Text -> m (Event t ())
rsvpChoiceButton label = do
  (btnEl, _) <- elAttr' "button" ("class" =: "rsvp-btn rsvp-choice-btn" <> "type" =: "button") $ text label
  pure (() <$ domEvent Click btnEl)

-- ── RSVP submission status ────────────────────────────────────────────────────

data RsvpStatus = StatusIdle | StatusSending | StatusSuccess | StatusError Text
  deriving (Eq)

statusBtnLabel :: RsvpStatus -> Text
statusBtnLabel s = case s of
  StatusIdle    -> "Enviar confirmaci\243n \8594"
  StatusSending -> "Enviando\8230"
  StatusSuccess -> "\161Enviado!"
  StatusError _ -> "Reintentar"

statusVisible :: RsvpStatus -> Bool
statusVisible StatusIdle = False
statusVisible _          = True

statusMsg :: RsvpStatus -> Text
statusMsg s = case s of
  StatusIdle    -> ""
  StatusSending -> "Enviando confirmaci\243n\8230"
  StatusSuccess -> "\161Respuesta recibida! Gracias."
  StatusError msg -> msg

xhrSuccess :: XhrResponse -> Bool
xhrSuccess resp = let s = _xhrResponse_status resp in s >= 200 && s < 300

xhrErrorText :: XhrResponse -> Text
xhrErrorText resp =
  case T.strip <$> _xhrResponse_responseText resp of
    Just msg | not (T.null msg) -> T.dropAround (== '"') msg
    _ -> "Hubo un problema al enviar. Revisa tu informaci\243n e int\233ntalo de nuevo."

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
      elAttr "div" ("class" =: "glass rect registry-card" <> "data-reveal" =: "") $ do
        elAttr "p" ("class" =: "mesa-label") $ text "LIVERPOOL"
        elAttr "p" ("class" =: "registry-number") $ text "51981423"
        qrBlock True
          "https://mesaderegalos.liverpool.com.mx/milistaderegalos/51981423"
          "qr-registry.png"
          "Ver mesa de regalos"

-- | A button link plus a QR code that is itself the same link. External
-- destinations open in a new tab; on-site ones (/fotos) stay in this tab.
qrBlock :: DomBuilder t m => Bool -> Text -> Text -> Text -> m ()
qrBlock newTab url image label =
  elAttr "div" ("class" =: "qr-block") $ do
    elAttr "a"
      ( "class" =: "rsvp-btn registry-link-btn"
     <> "href" =: url
     <> targetAttrs
      ) $ text label
    elAttr "a"
      ( "href" =: url
     <> targetAttrs
     <> "aria-label" =: label
      ) $
      elAttr "img"
        ( "class" =: "qr-img"
       <> "src" =: image
       <> "alt" =: "QR"
       <> "loading" =: "lazy"
        ) blank
  where
    targetAttrs
      | newTab    = "target" =: "_blank" <> "rel" =: "noopener noreferrer"
      | otherwise = mempty

-- ── FOTOS Y VIDEOS ───────────────────────────────────────────────────────────
-- The one place guests share photos and videos. The QR (printed on the tables too) and the
-- button both open /fotos; the live gallery band follows this section.

fotosSection :: DomBuilder t m => m ()
fotosSection =
  secImage "fotos" $ do
    elAttr "img"
      ( "class"   =: "section-img"
     <> "src"     =: "images/1.png"
     <> "alt"     =: ""
     <> "loading" =: "lazy"
      ) blank
    elAttr "div" ("class" =: "section-overlay") $ do
      elAttr "p" ("class" =: "label label-center" <> "data-reveal" =: "") $
        text "FOTOS Y VIDEOS"
      elAttr "div" ("class" =: "glass rect fotos-invite-card" <> "data-reveal" =: "") $ do
        elAttr "p" ("class" =: "mesa-label") $ text "EN CALIDAD ORIGINAL"
        elAttr "p" ("class" =: "fotos-invite-copy") $ text "Aparecer\225n aqu\237 abajo, en vivo."
        qrBlock False "/fotos" "qr-fotos.png" "Subir fotos y videos"

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
  , "  --photo-frame-height: max(600px, 100svh);"
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
  , "  .section-overlay { padding-top: .6rem; }"
  , "}"
  , "@media (orientation: portrait) and (max-width: 760px) {"
  , "  .section.image-section { overflow: hidden; }"
  , "  :root {"
  , "    --photo-frame-width: min(100vw, 390px);"
  , "    --photo-frame-height: min(140vw, 546px);"
  , "  }"
  , "  #hero .hero-bg {"
  , "    background-size: min(100vw, 390px) auto;"
  , "    background-position: center center;"
  , "  }"
  , "}"
  , "@media (orientation: portrait) and (max-width: 760px) and (max-height: 600px) {"
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
  , "  width: var(--photo-frame-width);"
  , "  height: var(--photo-frame-height);"
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
  , "  top: 50%;"
  , "  left: 50%;"
  , "  width: var(--photo-frame-width);"
  , "  height: var(--photo-frame-height);"
  , "  aspect-ratio: 1429 / 2000;"
  , "  transform: translate(-50%, -50%);"
  , "  z-index: 2;"
  , "  padding: 1.5rem 1.8rem var(--card-bottom-gap);"
  , "  background: linear-gradient(to top, rgba(28,20,16,.78) 0%, rgba(28,20,16,.30) 65%, transparent 100%);"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  align-items: center;"
  , "  justify-content: flex-end;"
  , "  overflow: hidden;"
  , "}"
  , ""

  -- Section background fallbacks
  , "#hero         { background-color: #3d2e22; }"
  , "#ubicacion    { background-color: #3a2c18; }"
  , "#dress-code   { background-color: #4a3010; }"
  , "#rsvp         { background: radial-gradient(ellipse at 50% 30%, #3a2614 0%, #1c1410 70%); }"
  , "#mesa-regalos { background-color: #382e24; }"
  , "#fotos        { background-color: #2f241b; }"
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
  , "  padding: 0 1.2rem clamp(5rem, 8vw, 8rem);"
  , "  position: relative;"
  , "  z-index: 1;"
  , "}"
  , ".hero-date {"
  , "  letter-spacing: .2em;"
  , "  font-size: clamp(.7rem, 2.1vw, 1.3rem);"
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
  , ".fixed-nav-link:focus-visible { outline: 1px solid rgba(212,180,131,.8); outline-offset: 4px; }"
  -- The track is layout-transparent on wide screens. On phones it becomes one
  -- swipeable row instead of wrapping; its edges fade out and
  -- navHighlightingJS keeps the active link centred.
  , ".fixed-nav-track { display: contents; }"
  , "@media (max-width: 760px) {"
  , "  .fixed-nav { padding-left: 0; padding-right: 0; }"
  , "  .fixed-nav-track {"
  , "    display: flex;"
  , "    flex: 1 1 auto;"
  , "    min-width: 0;"
  , "    flex-wrap: nowrap;"
  , "    justify-content: flex-start;"
  , "    justify-content: safe center;"
  , "    gap: inherit;"
  , "    padding: 0 clamp(1.2rem, 3.8vw, 3.3rem);"
  , "    overflow-x: auto;"
  , "    overscroll-behavior-x: contain;"
  , "    scrollbar-width: none;"
  , "    -webkit-mask-image: linear-gradient(90deg, transparent 0, #000 1.1rem, #000 calc(100% - 1.1rem), transparent 100%);"
  , "    mask-image: linear-gradient(90deg, transparent 0, #000 1.1rem, #000 calc(100% - 1.1rem), transparent 100%);"
  , "  }"
  , "  .fixed-nav-track::-webkit-scrollbar { display: none; }"
  , "  .fixed-nav-link { flex: 0 0 auto; }"
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
  , ".section-overlay .glass {"
  , "  align-self: center;"
  , "  max-height: calc(var(--photo-frame-height) - var(--card-bottom-gap) - 5.25rem);"
  , "  overflow-y: auto;"
  , "  scrollbar-gutter: stable;"
  , "  scrollbar-width: none;"
  , "  -ms-overflow-style: none;"
  , "  overscroll-behavior: contain;"
  , "}"
  , ".section-overlay .glass::-webkit-scrollbar { display: none; width: 0; height: 0; }"
  , ".card-scroll-indicator { position: absolute; width: .34rem; border-radius: 999px; background: rgba(255,255,255,.14); box-shadow: 0 0 0 1px rgba(0,0,0,.10); opacity: 0; pointer-events: none; transition: opacity .18s ease; z-index: 4; }"
  , ".card-scroll-indicator.is-visible { opacity: 1; }"
  , ".card-scroll-indicator-thumb { position: absolute; inset: 0 0 auto; border-radius: inherit; background: rgba(255,255,255,.62); box-shadow: 0 0 10px rgba(255,255,255,.18); }"
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
  , ".ubicacion-card .qr-block { margin-top: .65rem; }"
  , ""

  -- ── Dress code ────────────────────────────────────────────────────────────
  , ".dress-info { margin: 1.1rem auto; text-align: center; width: min(var(--card-width), calc(var(--photo-frame-width) - 1.2rem)); }"
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
  , "  justify-content: center;"
  , "  gap: 1.8rem;"
  , "  width: fit-content;"
  , "  margin: 1rem auto 1.4rem;"
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
  , ".rsvp-inline { display: grid; gap: .72rem; }"
  , ".rsvp-adults-note { color: #ffdfb4; line-height: 1.55; }"
  , ".rsvp-check { display: block; color: rgba(255,255,255,.9); line-height: 1.6; }"
  , ".rsvp-check input { width: auto; margin-right: .35rem; }"
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

  -- ── Mesa de Regalos ───────────────────────────────────────────────────────
  , ".registry-card { line-height: 1.7; text-align: center; margin: 1.1rem auto; transform: translateY(var(--card-lift)); }"
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
  , ".qr-block { display: grid; justify-items: center; gap: .8rem; }"
  , ".qr-img { width: min(132px, 42vw); height: auto; padding: .45rem; border-radius: 12px; background: rgba(255,255,255,.92); box-shadow: 0 10px 32px rgba(0,0,0,.28); }"
  , ""

  -- ── Itinerario — paper card on a candle-lit table ─────────────────────────
  -- overflow: clip (not auto/hidden) so the section is not a scroll container
  -- and the card's view() timeline follows the page scroll.
  , "#itinerario {"
  , "  --itin-nav: clamp(2.9rem, calc(6.8vw + .4rem), 6.4rem);"
  , "  overflow: clip;"
  , "}"
  -- Wide screens: also clear the floating back-to-top button.
  , "@media (min-width: 761px) {"
  , "  #itinerario { --itin-nav: calc(clamp(2.9rem, calc(6.8vw + .4rem), 6.4rem) + 3.6rem); }"
  , "}"
  , "#itinerario {"
  , "  background:"
  , "    radial-gradient(ellipse 52% 40% at 50% 46%, rgba(212,180,131,.17) 0%, rgba(212,180,131,.06) 48%, transparent 74%),"
  , "    radial-gradient(ellipse 34% 26% at 50% 40%, rgba(255,214,150,.08) 0%, transparent 70%),"
  , "    radial-gradient(ellipse 120% 95% at 50% 50%, transparent 42%, rgba(8,5,2,.62) 100%),"
  , "    #1c1410;"
  , "}"
  , "#itinerario::after {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  z-index: 0;"
  , "  pointer-events: none;"
  , "  opacity: .07;"
  , "  background-image: url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='180' height='180'><filter id='n'><feTurbulence type='fractalNoise' baseFrequency='.82' numOctaves='3' stitchTiles='stitch'/><feColorMatrix values='0 0 0 0 1 0 0 0 0 .93 0 0 0 0 .82 0 0 0 1 0'/></filter><rect width='100%' height='100%' filter='url(%23n)'/></svg>\");"
  , "}"
  , ".itinerario-stage {"
  , "  position: relative;"
  , "  z-index: 1;"
  , "  width: min(100vw, var(--photo-frame-width));"
  , "  height: 100%;"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "  gap: clamp(.6rem, 1.6svh, 1.2rem);"
  , "  padding-bottom: var(--itin-nav);"
  , "}"
  , ".itinerario-stage .label { padding-top: 0; }"
  , ".itinerario-link {"
  , "  display: flex;"
  , "  flex-direction: column;"
  , "  align-items: center;"
  , "  gap: clamp(1.1rem, 2.6svh, 1.7rem);"
  , "  color: inherit;"
  , "  text-decoration: none;"
  , "  outline: none;"
  , "  -webkit-tap-highlight-color: transparent;"
  , "}"
  , ".itinerario-settle { display: block; }"
  , ".itinerario-card {"
  , "  position: relative;"
  , "  display: block;"
  , "  width: min(calc(min(100vw, var(--photo-frame-width)) * .84), calc((max(600px, 100svh) - var(--itin-nav) - 9rem) * .7279));"
  , "  aspect-ratio: 1070 / 1470;"
  , "  border-radius: 3px;"
  , "  background: #f2eee3;"
  , "  box-shadow:"
  , "    0 1px 1px rgba(10,6,2,.35),"
  , "    0 6px 14px rgba(10,6,2,.30),"
  , "    0 24px 48px rgba(10,6,2,.38),"
  , "    0 0 90px rgba(212,180,131,.10);"
  , "  outline: 1px solid rgba(212,180,131,.30);"
  , "  outline-offset: 10px;"
  , "  transform: rotate(-.6deg);"
  , "  transition: transform .9s cubic-bezier(.19,1,.22,1);"
  , "}"
  -- Deeper lift shadow, faded in on hover (opacity only).
  , ".itinerario-card::before {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  z-index: -1;"
  , "  border-radius: inherit;"
  , "  box-shadow: 0 18px 30px rgba(10,6,2,.32), 0 46px 90px rgba(10,6,2,.45);"
  , "  opacity: 0;"
  , "  transition: opacity .6s cubic-bezier(.19,1,.22,1);"
  , "}"
  , ".itinerario-img {"
  , "  display: block;"
  , "  width: 100%;"
  , "  height: 100%;"
  , "  border-radius: inherit;"
  , "  user-select: none;"
  , "}"
  , ".itinerario-sheen {"
  , "  position: absolute;"
  , "  inset: 0;"
  , "  overflow: hidden;"
  , "  border-radius: inherit;"
  , "  pointer-events: none;"
  , "}"
  , ".itinerario-sheen::after {"
  , "  content: '';"
  , "  position: absolute;"
  , "  inset: -10% -40%;"
  , "  background: linear-gradient(112deg, transparent 38%, rgba(255,249,232,.55) 50%, transparent 62%);"
  , "  mix-blend-mode: soft-light;"
  , "  opacity: 0;"
  , "  transform: translateX(-70%);"
  , "}"
  , ".itinerario-hint {"
  , "  display: inline-flex;"
  , "  align-items: center;"
  , "  gap: .85rem;"
  , "  font-size: clamp(.62rem, .7vw, .78rem);"
  , "  letter-spacing: .3em;"
  , "  color: rgba(240,235,224,.58);"
  , "  transition: color .3s;"
  , "}"
  , ".itinerario-hint::before, .itinerario-hint::after {"
  , "  content: '';"
  , "  width: 1.6rem;"
  , "  height: 1px;"
  , "  background: rgba(212,180,131,.45);"
  , "}"
  , "@media (hover: hover) {"
  , "  .itinerario-link:hover .itinerario-card { transform: rotate(0deg) translateY(-5px); }"
  , "  .itinerario-link:hover .itinerario-card::before { opacity: 1; }"
  , "  .itinerario-link:hover .itinerario-hint { color: #d4b483; }"
  , "}"
  , ".itinerario-link:focus-visible .itinerario-card { outline: 2px solid #d4b483; outline-offset: 10px; transform: rotate(0deg); }"
  , ".itinerario-link:focus-visible .itinerario-hint { color: #d4b483; }"
  , ".itinerario-link:active .itinerario-card { transform: rotate(0deg) translateY(-1px) scale(.99); transition-duration: .15s; }"
  -- Time-based, played once when the card scrolls into view (settleOnViewJS).
  -- A scroll-linked timeline here jumped whenever a phone's toolbar
  -- showed or hid, i.e. on every change of scroll direction.
  , "@media (prefers-reduced-motion: no-preference) {"
  , "  .itinerario-settle.is-in { transition: opacity .9s ease-out, transform 1.4s cubic-bezier(.19,1,.22,1); }"
  , "  .itinerario-settle.is-armed:not(.is-in) { opacity: 0; transform: translateY(9%) rotate(-3.2deg) scale(.93); }"
  , "  .itinerario-settle.is-in .itinerario-sheen::after { animation: itinSheen 1.5s ease-in-out .55s both; }"
  , "}"
  , "@keyframes itinSheen {"
  , "  0%   { opacity: 0; transform: translateX(-70%); }"
  , "  25%  { opacity: 1; }"
  , "  75%  { opacity: 1; }"
  , "  100% { opacity: 0; transform: translateX(70%); }"
  , "}"
  , ".rsvp-status.is-error { color: #ffb4a8; }"
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
  , "  bottom: 4.55rem;"
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
  , "@media (min-width: 761px) {"
  , "  .back-to-top {"
  , "    bottom: clamp(6rem, 8.2vw, 9.25rem);"
  , "    right: max(1.6rem, calc((100vw - var(--photo-frame-width)) / 2 + 1rem));"
  , "  }"
  , "}"
  , ""

  -- ── Reduced motion ────────────────────────────────────────────────────────
  -- Phones resize the viewport as their toolbar shows/hides on every change
  -- of scroll direction, which makes view() timelines on the page scroller
  -- jump. Touch screens drop the two that track it: the section dim and the
  -- itinerary label (the other reveals track their own overlay and are stable).
  , "@media (hover: none) and (pointer: coarse) {"
  , "  .image-section { animation: none; }"
  , "  #itinerario [data-reveal] { animation: none; opacity: 1; transform: none; }"
  , "}"
  , "@media (prefers-reduced-motion: reduce) {"
  , "  .intro { display: none !important; }"
  , "  .progress-bar { display: none; }"
  , "  .intro-word, .intro-rule, .intro-sign { animation: none; opacity: 1; transform: none; }"
  , "  .hero-date { animation: none; opacity: 1; transform: none; }"
  , "  .marquee-track { animation: none; }"
  , "  [data-reveal] { animation: none !important; opacity: 1; transform: none; }"
  , "  .fixed-nav { opacity: 1; transform: none; }"
  , "  .itinerario-settle { transition: none; opacity: 1; transform: none; }"
  , "  .itinerario-sheen::after { animation: none !important; opacity: 0; }"
  , "  .itinerario-card, .itinerario-card::before { transition: none; }"
  , "}"
  ]
