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
import Wedding.Types (AttendanceStatus (..), RsvpRequest (..))

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
    rsvpSection
    ubicacionSection
    dressCodeSection
    mesaRegalosSection
    videoOpenE' <- videoMsgSection
    fixedNav
    backToTop
    pure videoOpenE'
  videoUploadOverlay videoOpenE
  pb <- getPostBuild
  performEvent_ $ liftJSM (void $ eval (navHighlightingJS <> ";" <> cardScrollIndicatorsJS <> ";" <> rsvpInlinePrefillJS <> ";" <> videoUploadGuestJS)) <$ pb

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

videoUploadGuestJS :: String
videoUploadGuestJS =
  "(function(){"
  <> "function clean(t){return (t||'').replace(/^\\\"|\\\"$/g,'');}"
  <> "function code(){return new URLSearchParams(location.search||'').get('code')||'';}"
  <> "function status(m,e){var s=document.getElementById('video-upload-status');if(s){s.textContent=m||'';s.classList.toggle('is-error',!!e);}}"
  <> "function enable(){var b=document.getElementById('video-upload-open');if(b){b.classList.remove('is-disabled');b.setAttribute('aria-disabled','false');}}"
  <> "function progress(v,show){var wrap=document.getElementById('video-upload-progress');var bar=document.getElementById('video-upload-progress-bar');var txt=document.getElementById('video-upload-progress-text');var n=Math.max(0,Math.min(100,Math.round(v||0)));if(wrap){wrap.hidden=!show;wrap.classList.toggle('is-error',false);}if(bar){bar.style.width=n+'%';bar.setAttribute('aria-valuenow',String(n));}if(txt)txt.textContent=n+'%';}"
  <> "function fail(m){var wrap=document.getElementById('video-upload-progress');if(wrap)wrap.classList.add('is-error');status(m,true);}"
  <> "function upload(){var f=document.getElementById('video-upload-form');if(!f||f.dataset.guestReady)return;f.dataset.guestReady='1';f.addEventListener('submit',function(e){e.preventDefault();var file=document.getElementById('video-upload-file');if(!file||!file.files||!file.files.length){fail('Selecciona un video.');return;}var data=new FormData(f);data.set('file',file.files[0],file.files[0].name);var c=code();if(c)data.set('invitationCode',c);var xhr=new XMLHttpRequest();xhr.open('POST','/api/videos');xhr.withCredentials=true;xhr.upload.onprogress=function(ev){if(ev.lengthComputable)progress((ev.loaded/ev.total)*100,true);};xhr.onload=function(){if(xhr.status>=200&&xhr.status<300){progress(100,true);status('Video recibido. Gracias por enviarlo.',false);f.reset();}else{fail(clean(xhr.responseText)||'No se pudo subir el video. Intentalo de nuevo.');}};xhr.onerror=function(){fail('No se pudo subir el video. Intentalo de nuevo.');};progress(0,true);status('Subiendo video...',false);xhr.send(data);});}"
  <> "function start(){enable();upload();}"
  <> "start();var n=0,t=setInterval(function(){start();if(++n>100)clearInterval(t);},50);"
  <> "})()"

videoUploadShimJS :: String
videoUploadShimJS =
  ""
{-
  "(function(){var identified=false;function code(){var p=new URLSearchParams(location.search||'');var c=p.get('code')||'';try{if(c)localStorage.setItem('weddingInvitationCode',c);else c=localStorage.getItem('weddingInvitationCode')||'';}catch(e){}return c;}function mark(v){identified=!!v;window.__weddingRsvpIdentified=identified;var b=document.getElementById('video-upload-open');if(b){b.classList.toggle('is-disabled',!identified);b.setAttribute('aria-disabled',identified?'false':'true');}}function buttonMessage(){var b=document.getElementById('video-upload-open');if(!b)return null;var m=document.getElementById('video-login-message');if(!m){m=document.createElement('p');m.id='video-login-message';m.className='video-login-message';b.parentNode.insertBefore(m,b.nextSibling);}return m;}function setButtonMessage(t){var m=buttonMessage();if(m)m.textContent=t||'';}function fillCode(){var c=code();var i=document.getElementById('rsvp-invitation-code');if(i&&c&&i.value!==c){i.value=c;i.dispatchEvent(new Event('input',{bubbles:true}));}return c;}function checkSession(){return fetch('/api/rsvp/me',{credentials:'same-origin'}).then(function(r){mark(r.ok);return r.ok;}).catch(function(){mark(false);return false;});}function rsvpLogin(){var c=code();if(!c||window.__weddingRsvpLoginTried)return;window.__weddingRsvpLoginTried=1;fetch('/api/rsvp/login',{method:'POST',credentials:'same-origin',headers:{'Content-Type':'application/json'},body:JSON.stringify({code:c})}).then(function(r){if(r.ok){mark(true);setButtonMessage('');}}).catch(function(){});}function watchRsvpSubmit(){if(window.__weddingRsvpSubmitWatchReady)return;window.__weddingRsvpSubmitWatchReady=1;var open=XMLHttpRequest.prototype.open;XMLHttpRequest.prototype.open=function(m,u){this.__weddingRsvpUrl=String(u||'');return open.apply(this,arguments);};var send=XMLHttpRequest.prototype.send;XMLHttpRequest.prototype.send=function(){if(this.__weddingRsvpUrl==='/api/rsvp')this.addEventListener('load',function(){if(this.status>=200&&this.status<300){mark(true);setButtonMessage('');}});return send.apply(this,arguments);};}function openRsvpFromLink(){if(window.__weddingRsvpAutoOpened||!code()||location.hash!=='#rsvp')return;var b=document.getElementById('rsvp-open');if(!b)return;window.__weddingRsvpAutoOpened=1;b.click();setTimeout(fillCode,80);}function start(){watchRsvpSubmit();fillCode();openRsvpFromLink();var openBtn=document.getElementById('video-upload-open');if(openBtn&&!openBtn.dataset.gated){openBtn.dataset.gated='1';openBtn.addEventListener('click',function(ev){if(!identified){ev.preventDefault();ev.stopImmediatePropagation();setButtonMessage('Primero identificate en la seccion RSVP con tu invitacion.');var r=document.getElementById('rsvp');if(r)r.scrollIntoView({behavior:'smooth'});}},true);}var f=document.getElementById('video-upload-form');if(!f||f.dataset.ready)return;f.dataset.ready='1';var s=document.getElementById('video-upload-status');var b=document.getElementById('video-upload-submit');function set(m,e){s.textContent=m||'';s.classList.toggle('is-error',!!e);}f.addEventListener('submit',function(ev){ev.preventDefault();var file=document.getElementById('video-upload-file').files[0];if(!file){set('Elige un video primero.',true);return;}b.disabled=true;fetch('/api/rsvp/me',{credentials:'same-origin'}).then(function(r){if(!r.ok)throw new Error('no rsvp');mark(true);var d=new FormData(f);set('Subiendo...',false);return fetch('/api/videos',{method:'POST',body:d,credentials:'same-origin'});}).then(function(r){if(!r.ok)throw new Error(String(r.status));return r.json();}).then(function(){f.reset();set('Video recibido. Gracias.',false);}).catch(function(){mark(false);set('Confirma tu RSVP con tu codigo de invitacion antes de subir video.',true);}).finally(function(){b.disabled=false;});});}mark(false);checkSession();rsvpLogin();start();var n=0,t=setInterval(function(){start();if(++n>200)clearInterval(t);},50);})()"
-}

rsvpInlinePrefillJS :: String
rsvpInlinePrefillJS =
  "(function(){function set(id,v){var el=document.getElementById(id);if(el&&el.value!==v){el.value=v;el.dispatchEvent(new Event('input',{bubbles:true}));}}function start(){var c=new URLSearchParams(location.search||'').get('code')||'';if(!c){set('rsvp-invitation-code','');return;}if(window.__weddingRsvpInlinePrefill)return;window.__weddingRsvpInlinePrefill=1;fetch('/api/invite?code='+encodeURIComponent(c),{credentials:'same-origin'}).then(function(r){if(!r.ok)throw new Error(String(r.status));return r.json();}).then(function(i){set('rsvp-invitation-code',c);set('rsvp-name',i.name||'');}).catch(function(){set('rsvp-invitation-code','');});}start();var n=0,t=setInterval(function(){start();if(++n>100)clearInterval(t);},50);})()"

videoUploadFixJS :: String
videoUploadFixJS =
{-
  "(function(){function txt(t){return (t||'').replace(/^\\\"|\\\"$/g,'');}function setStatus(m,e){var s=document.getElementById('video-upload-status');if(s){s.textContent=m||'';s.classList.toggle('is-error',!!e);}}function setLoginMessage(m){var b=document.getElementById('video-upload-open');if(!b)return;var p=document.getElementById('video-login-message');if(!p){p=document.createElement('p');p.id='video-login-message';p.className='video-login-message';b.parentNode.insertBefore(p,b.nextSibling);}p.textContent=m||'';}function rsvpStatus(m){setTimeout(function(){var xs=document.querySelectorAll('#rsvp-overlay .rsvp-status');for(var i=0;i<xs.length;i++){if(xs[i].offsetParent!==null){xs[i].textContent=m;xs[i].style.display='';xs[i].classList.add('is-error');}}},25);}function code(){var p=new URLSearchParams(location.search||'');return p.get('code')||'';}function enhanceRsvpErrors(){if(window.__weddingRsvpErrorsEnhanced)return;window.__weddingRsvpErrorsEnhanced=1;var open=XMLHttpRequest.prototype.open;XMLHttpRequest.prototype.open=function(m,u){this.__weddingRsvpFixUrl=String(u||'');return open.apply(this,arguments);};var send=XMLHttpRequest.prototype.send;XMLHttpRequest.prototype.send=function(){if(this.__weddingRsvpFixUrl==='/api/rsvp')this.addEventListener('load',function(){if(this.status>=400)rsvpStatus(txt(this.responseText)||'Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto.');});return send.apply(this,arguments);};}function enhanceInviteLogin(){var c=code();if(!c||window.__weddingRsvpLoginMessageTried)return;window.__weddingRsvpLoginMessageTried=1;fetch('/api/rsvp/login',{method:'POST',credentials:'same-origin',headers:{'Content-Type':'application/json'},body:JSON.stringify({code:c})}).then(function(r){if(!r.ok)setLoginMessage('Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto.');});}function enhanceUpload(){var f=document.getElementById('video-upload-form');if(!f||f.dataset.fixReady)return;f.dataset.fixReady='1';var b=document.getElementById('video-upload-submit');f.addEventListener('submit',function(ev){ev.preventDefault();ev.stopImmediatePropagation();var input=document.getElementById('video-upload-file');var file=input&&input.files&&input.files[0];if(!file){setStatus('Elige un video primero.',true);return;}if(b)b.disabled=true;setStatus('Subiendo...',false);var controller=window.AbortController?new AbortController():null;var timer=setTimeout(function(){if(controller)controller.abort();},120000);fetch('/api/rsvp/me',{credentials:'same-origin',signal:controller&&controller.signal}).then(function(r){if(!r.ok)throw new Error('Confirma tu RSVP con tu codigo de invitacion antes de subir video.');var d=new FormData(f);d.delete('name');return fetch('/api/videos',{method:'POST',body:d,credentials:'same-origin',signal:controller&&controller.signal});}).then(function(r){if(!r.ok)return r.text().then(function(t){throw new Error(txt(t)||'No pudimos subir el video. Intentalo de nuevo.');});return r.json();}).then(function(){f.reset();setStatus('Video recibido. Gracias.',false);}).catch(function(e){setStatus(e&&e.name==='AbortError'?'La subida tardo demasiado. Intentalo con un video mas pequeno.':(e&&e.message)||'No pudimos subir el video. Intentalo de nuevo.',true);}).finally(function(){clearTimeout(timer);if(b)b.disabled=false;});},true);}function start(){enhanceRsvpErrors();enhanceInviteLogin();enhanceUpload();}start();var n=0,t=setInterval(function(){start();if(++n>200)clearInterval(t);},50);})()"

videoUploadProgressJS :: String
-}
  ""

videoUploadProgressJS :: String
videoUploadProgressJS =
{-
  "(function(){function clean(t){return (t||'').replace(/^\\\"|\\\"$/g,'');}function status(m,e){var s=document.getElementById('video-upload-status');if(s){s.textContent=m||'';s.classList.toggle('is-error',!!e);}}function progress(v,show){var wrap=document.getElementById('video-upload-progress');var bar=document.getElementById('video-upload-progress-bar');var txt=document.getElementById('video-upload-progress-text');var n=Math.max(0,Math.min(100,Math.round(v||0)));if(wrap){wrap.hidden=!show;wrap.classList.toggle('is-error',false);}if(bar){bar.style.width=n+'%';bar.setAttribute('aria-valuenow',String(n));}if(txt)txt.textContent=n+'%';}function fail(m){var wrap=document.getElementById('video-upload-progress');if(wrap)wrap.classList.add('is-error');status(m,true);}function loginMessage(m){var b=document.getElementById('video-upload-open');if(!b)return;var p=document.getElementById('video-login-message');if(!p){p=document.createElement('p');p.id='video-login-message';p.className='video-login-message';b.parentNode.insertBefore(p,b.nextSibling);}p.textContent=m||'';}function code(){var p=new URLSearchParams(location.search||'');return p.get('code')||'';}function rsvpStatus(m){setTimeout(function(){var xs=document.querySelectorAll('#rsvp-overlay .rsvp-status');for(var i=0;i<xs.length;i++){if(xs[i].offsetParent!==null){xs[i].textContent=m;xs[i].style.display='';xs[i].classList.add('is-error');}}},25);}function enhanceRsvpErrors(){if(window.__weddingRsvpProgressErrors)return;window.__weddingRsvpProgressErrors=1;var open=XMLHttpRequest.prototype.open;XMLHttpRequest.prototype.open=function(m,u){this.__weddingRsvpProgressUrl=String(u||'');return open.apply(this,arguments);};var send=XMLHttpRequest.prototype.send;XMLHttpRequest.prototype.send=function(){if(this.__weddingRsvpProgressUrl==='/api/rsvp')this.addEventListener('load',function(){if(this.status>=400)rsvpStatus(clean(this.responseText)||'Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto.');});return send.apply(this,arguments);};}function enhanceInviteLogin(){var c=code();if(!c||window.__weddingRsvpProgressLogin)return;window.__weddingRsvpProgressLogin=1;fetch('/api/rsvp/login',{method:'POST',credentials:'same-origin',headers:{'Content-Type':'application/json'},body:JSON.stringify({code:c})}).then(function(r){if(!r.ok)loginMessage('Codigo incorrecto. Revisa tu invitacion o pidenos el codigo correcto.');});}function uploadWithProgress(form,button){return new Promise(function(resolve,reject){var xhr=new XMLHttpRequest();var timeout=setTimeout(function(){xhr.abort();reject(new Error('La subida tardo demasiado. Intentalo con un video mas pequeno.'));},120000);xhr.open('POST','/api/videos');xhr.withCredentials=true;xhr.upload.onprogress=function(ev){if(ev.lengthComputable){var pct=ev.total?ev.loaded/ev.total*100:0;progress(pct,true);status('Subiendo... '+Math.round(pct)+'%',false);}else{progress(5,true);status('Subiendo...',false);}};xhr.onload=function(){clearTimeout(timeout);if(xhr.status>=200&&xhr.status<300){progress(100,true);resolve();}else{reject(new Error(clean(xhr.responseText)||'No pudimos subir el video. Intentalo de nuevo.'));}};xhr.onerror=function(){clearTimeout(timeout);reject(new Error('No pudimos subir el video. Revisa tu conexion e intentalo de nuevo.'));};xhr.onabort=function(){clearTimeout(timeout);reject(new Error('La subida fue cancelada. Intentalo de nuevo.'));};var data=new FormData(form);data.delete('name');xhr.send(data);});}function enhanceUpload(){var f=document.getElementById('video-upload-form');if(!f||f.dataset.progressReady)return;f.dataset.progressReady='1';f.dataset.fixReady='1';var b=document.getElementById('video-upload-submit');f.addEventListener('submit',function(ev){ev.preventDefault();ev.stopImmediatePropagation();var input=document.getElementById('video-upload-file');var file=input&&input.files&&input.files[0];if(!file){progress(0,false);status('Elige un video primero.',true);return;}if(b)b.disabled=true;progress(0,true);status('Preparando subida...',false);fetch('/api/rsvp/me',{credentials:'same-origin'}).then(function(r){if(!r.ok)throw new Error('Confirma tu RSVP con tu codigo de invitacion antes de subir video.');return uploadWithProgress(f,b);}).then(function(){f.reset();progress(100,true);status('Video recibido. Gracias.',false);}).catch(function(e){fail((e&&e.message)||'No pudimos subir el video. Intentalo de nuevo.');}).finally(function(){if(b)b.disabled=false;});},true);}function start(){enhanceRsvpErrors();enhanceInviteLogin();enhanceUpload();}start();var n=0,t=setInterval(function(){start();if(++n>200)clearInterval(t);},50);})()"

fixedNav :: DomBuilder t m => m ()
-}
  ""

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
        qrBlock "https://www.google.com/maps/search/?api=1&query=20.5229282,-100.4039031" "qr-location.png" "Abrir ubicaci\243n"
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
        qrBlock
          "https://mesaderegalos.liverpool.com.mx/milistaderegalos/51981423"
          "qr-registry.png"
          "Ver mesa de regalos"

qrBlock :: DomBuilder t m => Text -> Text -> Text -> m ()
qrBlock url image label =
  elAttr "div" ("class" =: "qr-block") $ do
    elAttr "a"
      ( "class" =: "rsvp-btn registry-link-btn"
     <> "href" =: url
     <> "target" =: "_blank"
     <> "rel" =: "noopener noreferrer"
      ) $ text label
    elAttr "a"
      ( "href" =: url
     <> "target" =: "_blank"
     <> "rel" =: "noopener noreferrer"
     <> "aria-label" =: label
      ) $
      elAttr "img"
        ( "class" =: "qr-img"
       <> "src" =: image
       <> "alt" =: "QR"
       <> "loading" =: "lazy"
        ) blank

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
      elAttr "p" ("class" =: "construction-copy") $
        text "M\225ndanos un video privado o p\250blico para proyectar en la boda, puedes mandar cuantos videos gustes pero procura que no pesen tanto por favor."
      elAttr "form" ("id" =: "video-upload-form" <> "class" =: "video-upload-form") $ do
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
        elAttr "div"
          ( "id" =: "video-upload-progress"
         <> "class" =: "video-upload-progress"
         <> "hidden" =: "hidden"
          ) $ do
          elAttr "div" ("class" =: "video-upload-progress-track") $
            elAttr "div"
              ( "id" =: "video-upload-progress-bar"
             <> "class" =: "video-upload-progress-bar"
             <> "role" =: "progressbar"
             <> "aria-valuemin" =: "0"
             <> "aria-valuemax" =: "100"
             <> "aria-valuenow" =: "0"
              ) blank
          elAttr "p" ("id" =: "video-upload-progress-text" <> "class" =: "video-upload-progress-text") $ text "0%"
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
  , ".video-wa-btn.is-disabled { opacity: .48; cursor: not-allowed; filter: grayscale(.35); }"
  , ".video-login-message { margin-top: .75rem; color: #ffdfb4; line-height: 1.55; font-size: .92rem; }"
  , ".video-upload-form { margin-top: 1.1rem; text-align: left; }"
  , ".video-upload-message { min-height: 6rem; resize: vertical; }"
  , ".video-upload-file { padding: .62rem; }"
  , ".video-upload-progress { margin: .4rem 0 1rem; }"
  , ".video-upload-progress[hidden] { display: none; }"
  , ".video-upload-progress-track { overflow: hidden; height: .55rem; border: 1px solid rgba(255,255,255,.36); border-radius: 999px; background: rgba(255,255,255,.1); }"
  , ".video-upload-progress-bar { width: 0%; height: 100%; border-radius: inherit; background: linear-gradient(90deg, rgba(255,255,255,.72), rgba(255,232,195,.96)); box-shadow: 0 0 18px rgba(255,232,195,.28); transition: width .18s ease-out; }"
  , ".video-upload-progress-text { margin: .45rem 0 0; color: rgba(255,255,255,.82); font-size: .88rem; letter-spacing: .06em; text-align: right; }"
  , ".video-upload-progress.is-error .video-upload-progress-bar { background: linear-gradient(90deg, rgba(255,180,168,.8), rgba(255,120,105,.95)); }"
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
