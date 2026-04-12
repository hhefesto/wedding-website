(function () {
  'use strict';

  // ── Bootstrap ────────────────────────────────────────────────────────────────
  // Reflex-DOM builds the DOM after runmain.js executes.
  // We wait for #closing (last section) to appear before initialising GSAP.

  function waitForReflex() {
    return new Promise(function (resolve) {
      if (document.querySelector('#closing')) { resolve(); return; }
      var obs = new MutationObserver(function (_, o) {
        if (document.querySelector('#closing')) { o.disconnect(); resolve(); }
      });
      obs.observe(document.body || document.documentElement,
        { childList: true, subtree: true });
    });
  }

  waitForReflex().then(initAll);

  // ── Init ─────────────────────────────────────────────────────────────────────

  function initAll() {
    gsap.registerPlugin(ScrollTrigger, ScrollToPlugin);

    var prefersReduced = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    if (prefersReduced) {
      var intro = document.getElementById('intro');
      if (intro) intro.style.display = 'none';
      initRSVP();
      initMesaRegalos();
      initBackToTop();
      return;
    }

    initProgressBar();
    initIntro();       // chains into playHero() on completion
    initMesaHorizontal();
    initMagnetic();
    initSectionReveals();
    initParallax();
    initBackToTop();
    initRSVP();
    initMesaRegalos();
  }

  // ── splitWords ───────────────────────────────────────────────────────────────
  // Manual replacement for the premium SplitText plugin.
  // Wraps each word (or char) in <span class="sw"><span class="sw-i">…</span></span>.
  // .sw has overflow:hidden; .sw-i is what we tween on the y axis.

  function splitWords(selector, mode) {
    mode = mode || 'word';
    document.querySelectorAll(selector).forEach(function (el) {
      var text = el.textContent;
      el.textContent = '';
      var units = mode === 'char' ? text.split('') : text.split(/(\s+)/);
      units.forEach(function (u) {
        if (/^\s+$/.test(u)) {
          el.appendChild(document.createTextNode('\u00a0'));
          return;
        }
        var outer = document.createElement('span');
        outer.className = 'sw';
        var inner = document.createElement('span');
        inner.className = 'sw-i';
        inner.textContent = u;
        outer.appendChild(inner);
        el.appendChild(outer);
      });
    });
  }

  // ── Progress bar ─────────────────────────────────────────────────────────────

  function initProgressBar() {
    var bar = document.getElementById('progress-bar');
    if (!bar) return;
    gsap.to(bar, {
      scaleX: 1,
      ease: 'none',
      scrollTrigger: {
        trigger: document.documentElement,
        start: 'top top',
        end: 'bottom bottom',
        scrub: 0.1
      }
    });
  }

  // ── Intro overlay ────────────────────────────────────────────────────────────

  function initIntro() {
    var introEl = document.getElementById('intro');
    if (!introEl) { playHero(); return; }

    // Once-per-session: skip the overlay if already seen
    if (sessionStorage.getItem('introSeen')) {
      introEl.style.display = 'none';
      playHero();
      return;
    }

    splitWords('.intro-kicker', 'word');

    // Set initial states via GSAP (more reliable than CSS on some browsers)
    gsap.set('.intro-kicker .sw-i', { y: '110%' });
    gsap.set('.intro-rule', { width: 0 });
    gsap.set('.intro-sign', { opacity: 0, y: 22 });
    gsap.set('#intro', { clipPath: 'inset(0% 0% 0% 0%)' });

    var tl = gsap.timeline({
      onComplete: function () {
        sessionStorage.setItem('introSeen', '1');
        playHero();
      }
    });

    tl.to('.intro-kicker .sw-i', {
      y: '0%', duration: 0.75, stagger: 0.07, ease: 'power3.out'
    })
    .to('.intro-rule', { width: '60vw', duration: 0.6, ease: 'power2.out' }, '-=0.3')
    .to('.intro-sign', { opacity: 1, y: 0, duration: 0.7, ease: 'power3.out' }, '-=0.2')
    .to({}, { duration: 0.75 }) // hold
    .to('.intro-inner', { opacity: 0, y: -20, duration: 0.45, ease: 'power2.in' })
    .to('#intro', {
      clipPath: 'inset(50% 0% 50% 0%)',
      duration: 0.9, ease: 'power4.inOut'
    }, '-=0.05')
    .set('#intro', { display: 'none' });

    // Skip on click anywhere in the intro
    introEl.addEventListener('click', function () {
      tl.progress(0.88);
    }, { once: true });
  }

  // ── Hero entrance ─────────────────────────────────────────────────────────────
  // Called after intro completes (or immediately on repeated sessions).

  function playHero() {
    // Bg image zooms from slightly-enlarged to natural size
    gsap.fromTo('.hero-bg',
      { scale: 1.08 },
      { scale: 1.0, duration: 1.3, ease: 'power2.out' }
    );

    splitWords('.hero-name', 'word');

    var tl = gsap.timeline({ delay: 0.1 });

    tl.to('.hero-name .sw-i', {
      y: '0%', duration: 0.85, stagger: 0.1, ease: 'expo.out',
      // words start at y:110% set by splitWords + GSAP set below
    });

    // Pre-set inner spans to 110% so they're hidden
    gsap.set('.hero-name .sw-i', { y: '110%' });
    tl.to('.hero-name .sw-i', {
      y: '0%', duration: 0.85, stagger: 0.1, ease: 'expo.out'
    }, 0);

    tl.to('.hero-date-rule', {
      width: '16vw', duration: 0.5, stagger: 0.1, ease: 'power2.out'
    }, '-=0.4')
    .from('.hero-date', {
      opacity: 0, duration: 0.45, ease: 'power2.out'
    }, '<')
    .from('.hero-nav a', {
      opacity: 0, y: 14, duration: 0.4, stagger: 0.07, ease: 'power2.out'
    }, '-=0.2');

    // Scroll-hint chevron bounce
    var hint = document.querySelector('.scroll-hint');
    if (hint) {
      gsap.to(hint, {
        y: 7, duration: 1.2, yoyo: true, repeat: -1, ease: 'sine.inOut'
      });
      window.addEventListener('scroll', function () {
        gsap.to(hint, { opacity: 0, duration: 0.3, ease: 'power2.in' });
      }, { once: true });
    }
  }

  // ── Section reveals ───────────────────────────────────────────────────────────

  function initSectionReveals() {
    initUbicacion();
    initDressCode();
    initRSVPSection();
    initMesaReveal();
    initVideoReveal();
    initClosing();
  }

  // ── Ubicación ────────────────────────────────────────────────────────────────

  function initUbicacion() {
    // Seamless marquee: the track has duplicated content; moving -50% loops cleanly
    gsap.to('.marquee-track', {
      xPercent: -50,
      duration: 32,
      ease: 'none',
      repeat: -1
    });

    gsap.from('#ubicacion .label', {
      scrollTrigger: { trigger: '#ubicacion', start: 'top 78%' },
      opacity: 0, y: -22, duration: 0.6, ease: 'power2.out'
    });

    gsap.from('#ubicacion .glass.blob', {
      scrollTrigger: { trigger: '#ubicacion', start: 'top 72%' },
      opacity: 0, scale: 0.91, duration: 1.1, ease: 'power3.out', delay: 0.12
    });

    // Breathing blob (runs forever after first enter)
    ScrollTrigger.create({
      trigger: '#ubicacion',
      start: 'top 90%',
      once: true,
      onEnter: function () {
        gsap.to('#ubicacion .glass.blob', {
          borderRadius: '48% 52% 42% 58% / 56% 40% 60% 44%',
          duration: 8, repeat: -1, yoyo: true, ease: 'sine.inOut'
        });
      }
    });
  }

  // ── Dress Code ────────────────────────────────────────────────────────────────

  function initDressCode() {
    if (!document.querySelector('#dress-code')) return;

    // Label chars rise (triggered independently of the scrub pin)
    splitWords('#dress-code .label', 'char');
    gsap.set('#dress-code .label .sw-i', { y: '110%' });
    gsap.to('#dress-code .label .sw-i', {
      scrollTrigger: { trigger: '#dress-code', start: 'top 80%' },
      y: '0%', duration: 0.6, stagger: 0.04, ease: 'power3.out'
    });

    // Pinned clip-path zoom — the signature technique
    var pinTl = gsap.timeline({
      scrollTrigger: {
        trigger: '#dress-code',
        start: 'top top',
        end: '+=120%',
        pin: true,
        scrub: 0.6
      }
    });

    gsap.set('.dress-bg', {
      clipPath: 'inset(14% 8% 14% 8% round 24px)',
      scale: 1.12
    });

    pinTl.to('.dress-bg', {
      clipPath: 'inset(0% 0% 0% 0% round 0px)',
      scale: 1.0,
      ease: 'none'
    });
    pinTl.from('.dress-suits', { x: -130, opacity: 0, ease: 'power3.out' }, 0);
    pinTl.from('.dress-gowns', { x:  130, opacity: 0, ease: 'power3.out' }, 0);
    pinTl.from('.dress-info',  { y: 44, opacity: 0, ease: 'power3.out' }, 0.25);
  }

  // ── RSVP ─────────────────────────────────────────────────────────────────────

  function initRSVPSection() {
    gsap.from('#rsvp .label', {
      scrollTrigger: { trigger: '#rsvp', start: 'top 78%' },
      opacity: 0, y: -22, duration: 0.6, ease: 'power2.out'
    });

    gsap.from('#rsvp .glass.rect p', {
      scrollTrigger: { trigger: '#rsvp', start: 'top 72%' },
      opacity: 0, y: 18, duration: 0.55, stagger: 0.12, ease: 'power2.out'
    });

    gsap.from('#rsvp-open-btn', {
      scrollTrigger: { trigger: '#rsvp', start: 'top 65%' },
      scale: 0, opacity: 0, duration: 0.7, ease: 'back.out(1.8)'
    });

    ScrollTrigger.create({
      trigger: '#rsvp',
      start: 'top 62%',
      once: true,
      onEnter: function () {
        gsap.to('#rsvp-open-btn', {
          boxShadow: '0 0 22px rgba(255,255,255,.35)',
          borderColor: 'rgba(255,255,255,.85)',
          duration: 0.55, yoyo: true, repeat: 3, ease: 'power2.inOut'
        });
      }
    });
  }

  // ── Mesa de Regalos (scroll reveal only — horizontal handled separately) ──────

  function initMesaReveal() {
    gsap.from('#mesa-regalos .label', {
      scrollTrigger: { trigger: '#mesa-regalos', start: 'top 78%' },
      opacity: 0, y: -22, duration: 0.6, ease: 'power2.out'
    });
  }

  // ── Video Mensaje ─────────────────────────────────────────────────────────────

  function initVideoReveal() {
    var mask = document.querySelector('.video-mask');
    if (!mask) return;

    gsap.set(mask, { clipPath: 'inset(0 0 100% 0)' });
    gsap.to(mask, {
      clipPath: 'inset(0 0 0% 0)',
      duration: 0.9, ease: 'power3.out',
      scrollTrigger: { trigger: '#video-mensaje', start: 'top 70%' }
    });

    gsap.from('#video-mensaje .label', {
      scrollTrigger: { trigger: '#video-mensaje', start: 'top 78%' },
      opacity: 0, y: -22, duration: 0.6, ease: 'power2.out'
    });

    gsap.from('.video-msg-icon, .video-msg-text, .video-wa-btn', {
      scrollTrigger: { trigger: '#video-mensaje', start: 'top 65%' },
      x: 28, opacity: 0, duration: 0.5, stagger: 0.12, ease: 'power2.out'
    });
  }

  // ── Closing ───────────────────────────────────────────────────────────────────

  function initClosing() {
    splitWords('.closing-line', 'word');
    gsap.set('.closing-line .sw-i', { y: '110%' });
    gsap.to('.closing-line .sw-i', {
      scrollTrigger: { trigger: '#closing', start: 'top 70%' },
      y: '0%', duration: 1.0, stagger: 0.12, ease: 'expo.out'
    });

    // Animated countdown to wedding day
    var daysEl = document.getElementById('countdown-days');
    if (daysEl) {
      var wedding = new Date('2026-10-10T18:00:00');
      var now = new Date();
      var diff = Math.max(0, Math.ceil((wedding - now) / 86400000));
      var proxy = { n: 0 };
      ScrollTrigger.create({
        trigger: '#closing',
        start: 'top 70%',
        once: true,
        onEnter: function () {
          gsap.to(proxy, {
            n: diff,
            duration: 1.4,
            ease: 'power2.out',
            onUpdate: function () {
              daysEl.textContent = Math.round(proxy.n);
            }
          });
        }
      });
    }
  }

  // ── Parallax ─────────────────────────────────────────────────────────────────

  function initParallax() {
    if (window.matchMedia('(max-width: 768px)').matches) return;

    ['.hero-bg', '.ubicacion-bg', '.mesa-bg', '.closing-bg'].forEach(function (sel) {
      var el = document.querySelector(sel);
      if (!el) return;
      var section = el.closest('.section');
      if (!section) return;
      gsap.to(el, {
        backgroundPositionY: '+=18%',
        ease: 'none',
        scrollTrigger: {
          trigger: section,
          start: 'top bottom',
          end: 'bottom top',
          scrub: true
        }
      });
    });
  }

  // ── Horizontal mesa scroll ────────────────────────────────────────────────────

  function initMesaHorizontal() {
    var track = document.querySelector('#mesa-regalos .h-track');
    if (!track) return;
    if (window.matchMedia('(max-width: 640px)').matches) return;

    gsap.to(track, {
      x: function () {
        return -(track.scrollWidth - window.innerWidth + 48);
      },
      ease: 'none',
      scrollTrigger: {
        trigger: '#mesa-regalos',
        start: 'top top',
        end: function () {
          return '+=' + Math.max(0, track.scrollWidth - window.innerWidth + 48);
        },
        pin: true,
        scrub: 0.6,
        invalidateOnRefresh: true
      }
    });
  }

  // ── Magnetic hover ────────────────────────────────────────────────────────────

  function initMagnetic() {
    document.querySelectorAll('[data-magnetic]').forEach(function (el) {
      el.addEventListener('mousemove', function (e) {
        var rect = el.getBoundingClientRect();
        var dx = e.clientX - (rect.left + rect.width  / 2);
        var dy = e.clientY - (rect.top  + rect.height / 2);
        gsap.to(el, {
          x: dx * 0.28, y: dy * 0.28,
          duration: 0.4, ease: 'power2.out'
        });
      });
      el.addEventListener('mouseleave', function () {
        gsap.to(el, { x: 0, y: 0, duration: 0.6, ease: 'elastic.out(1, 0.4)' });
      });
    });
  }

  // ── Back to top ───────────────────────────────────────────────────────────────

  function initBackToTop() {
    var btn = document.getElementById('back-to-top');
    if (!btn) return;

    var visible = false;

    ScrollTrigger.create({
      trigger: '#ubicacion',
      start: 'top 90%',
      onEnter: function () {
        if (visible) return;
        visible = true;
        btn.style.display = 'block';
        gsap.fromTo(btn,
          { opacity: 0, scale: 0.6, y: 14 },
          { opacity: 1, scale: 1, y: 0, duration: 0.32, ease: 'back.out(1.8)' });
      },
      onLeaveBack: function () {
        if (!visible) return;
        visible = false;
        gsap.to(btn, {
          opacity: 0, scale: 0.6, y: 14, duration: 0.35, ease: 'power2.in',
          onComplete: function () { btn.style.display = 'none'; }
        });
      }
    });

    btn.addEventListener('click', function () {
      gsap.to(btn, {
        scale: 0.88, duration: 0.12, ease: 'power2.in',
        onComplete: function () {
          gsap.to(btn, { scale: 1, duration: 0.2, ease: 'back.out(2)' });
        }
      });
      gsap.to(window, {
        scrollTo: { y: 0, autoKill: false },
        duration: 0.85,
        ease: 'power4.inOut'
      });
    });
  }

  // ── RSVP multi-step WhatsApp overlay ─────────────────────────────────────────

  function initRSVP() {
    var overlay  = document.getElementById('rsvp-overlay');
    var openBtn  = document.getElementById('rsvp-open-btn');
    var closeBtn = document.getElementById('rsvp-close');
    if (!overlay || !openBtn) return;

    var guestCount = 1;

    openBtn.addEventListener('click', function () {
      overlay.style.display = 'flex';
      gsap.fromTo(overlay,
        { opacity: 0 },
        { opacity: 1, duration: 0.35, ease: 'power2.out' });
      gsap.fromTo('.rsvp-modal',
        { scale: 0.90, opacity: 0, y: 24 },
        { scale: 1, opacity: 1, y: 0, duration: 0.4, ease: 'back.out(1.6)' });
      showStep(1);
    });

    function closeOverlay() {
      gsap.to('.rsvp-modal', {
        scale: 0.90, opacity: 0, y: 16, duration: 0.22, ease: 'power2.in'
      });
      gsap.to(overlay, {
        opacity: 0, duration: 0.28, delay: 0.1, ease: 'power2.in',
        onComplete: function () {
          overlay.style.display = 'none';
          guestCount = 1;
          showStep(1);
          updateCount();
        }
      });
    }

    if (closeBtn) closeBtn.addEventListener('click', closeOverlay);
    overlay.addEventListener('click', function (e) {
      if (e.target === overlay) closeOverlay();
    });

    function showStep(n) {
      var steps = document.querySelectorAll('.rsvp-step');
      steps.forEach(function (s) { s.style.display = 'none'; });
      var target = document.getElementById('rsvp-step-' + n);
      if (!target) return;
      target.style.display = 'block';
      gsap.from(target, { opacity: 0, x: 28, duration: 0.32, ease: 'power2.out' });
    }

    document.querySelectorAll('.rsvp-next').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var next = parseInt(btn.getAttribute('data-next'), 10);
        if (next === 4) buildSummary();
        showStep(next);
      });
    });

    function updateCount() {
      var el = document.getElementById('rsvp-count');
      if (el) el.textContent = guestCount;
    }

    var minusBtn = document.getElementById('rsvp-minus');
    var plusBtn  = document.getElementById('rsvp-plus');
    if (minusBtn) {
      minusBtn.addEventListener('click', function () {
        if (guestCount > 1) { guestCount--; updateCount(); animateCount(); }
      });
    }
    if (plusBtn) {
      plusBtn.addEventListener('click', function () {
        if (guestCount < 10) { guestCount++; updateCount(); animateCount(); }
      });
    }

    function animateCount() {
      gsap.from('#rsvp-count', { scale: 1.45, duration: 0.25, ease: 'back.out(2)' });
    }

    function buildSummary() {
      var nameEl    = document.getElementById('rsvp-name');
      var dietaryEl = document.getElementById('rsvp-dietary');
      var name    = nameEl    ? nameEl.value.trim()    : '';
      var dietary = dietaryEl ? dietaryEl.value.trim() : '';

      var summaryEl = document.getElementById('rsvp-summary');
      if (summaryEl) {
        summaryEl.innerHTML =
          '<p>Nombre: <strong>' + escHtml(name || '\u2014') + '</strong></p>' +
          '<p>Asistentes: <strong>' + guestCount + '</strong></p>' +
          (dietary
            ? '<p>Restricciones: <strong>' + escHtml(dietary) + '</strong></p>'
            : '');
      }

      var msg = '\u00a1Hola! Confirmo mi asistencia a la boda de Daniel y Ana Cristina \ud83c\udf89\n' +
        'Nombre: ' + (name || '\u2014') + '\n' +
        'Asistentes: ' + guestCount +
        (dietary ? '\nRestricciones: ' + dietary : '');

      var waBtn = document.getElementById('rsvp-whatsapp-btn');
      if (waBtn) {
        waBtn.href = 'https://wa.me/PLACEHOLDER?text=' + encodeURIComponent(msg);
      }
    }

    function escHtml(s) {
      return String(s)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
    }
  }

  // ── Mesa de Regalos — copy to clipboard ──────────────────────────────────────

  function initMesaRegalos() {
    document.querySelectorAll('.copy-btn').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var val = btn.getAttribute('data-copy');
        if (!val) return;
        navigator.clipboard.writeText(val).then(function () {
          var original = btn.textContent;
          gsap.to(btn, {
            opacity: 0.5, duration: 0.12,
            onComplete: function () {
              btn.textContent = '\u00a1Copiado!';
              gsap.to(btn, {
                opacity: 1, duration: 0.12,
                onComplete: function () {
                  setTimeout(function () {
                    gsap.to(btn, {
                      opacity: 0.5, duration: 0.12,
                      onComplete: function () {
                        btn.textContent = original;
                        gsap.to(btn, { opacity: 1, duration: 0.12 });
                      }
                    });
                  }, 1600);
                }
              });
            }
          });
        }).catch(function () {
          // Clipboard API unavailable — silent fallback
        });
      });
    });
  }

})();
