(function () {
  'use strict';

  // ── Bootstrap ─────────────────────────────────────────────────────────────
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

  // ── Init ──────────────────────────────────────────────────────────────────

  function initAll() {
    gsap.registerPlugin(ScrollTrigger, ScrollToPlugin);
    initHero();
    initScrollAnimations();
    initParallax();
    initBackToTop();
    initRSVP();
    initMesaRegalos();
  }

  // ── Hero entrance ─────────────────────────────────────────────────────────

  function initHero() {
    var tl = gsap.timeline({ delay: 0.25 });
    tl.from('.hero-names .hero-name', {
      opacity: 0, y: 44, duration: 1.0, stagger: 0.32, ease: 'power3.out'
    })
    .from('.hero-date', {
      opacity: 0, y: 18, duration: 0.6, ease: 'power2.out'
    }, '-=0.35')
    .from('.hero-nav a', {
      opacity: 0, y: 16, duration: 0.45, stagger: 0.08, ease: 'power2.out'
    }, '-=0.25');
  }

  // ── Scroll-triggered reveals ──────────────────────────────────────────────

  function initScrollAnimations() {

    // ── Ubicacion ────────────────────────────────────────────────────────────
    gsap.from('#ubicacion .label', {
      scrollTrigger: { trigger: '#ubicacion', start: 'top 78%' },
      opacity: 0, y: -22, duration: 0.6, ease: 'power2.out'
    });
    gsap.from('#ubicacion .glass.blob', {
      scrollTrigger: { trigger: '#ubicacion', start: 'top 72%' },
      opacity: 0, scale: 0.91, duration: 1.1, ease: 'power3.out', delay: 0.12
    });
    // Breathing animation on blob (runs forever)
    gsap.to('#ubicacion .glass.blob', {
      borderRadius: '48% 52% 42% 58% / 56% 40% 60% 44%',
      duration: 8, repeat: -1, yoyo: true, ease: 'sine.inOut'
    });

    // ── Dress Code ────────────────────────────────────────────────────────────
    gsap.from('#dress-code .label', {
      scrollTrigger: { trigger: '#dress-code', start: 'top 78%' },
      opacity: 0, y: -28, duration: 0.65, ease: 'power2.out'
    });
    gsap.from('#dress-code .glass.rect', {
      scrollTrigger: { trigger: '#dress-code', start: 'top 72%' },
      opacity: 0, x: -52, duration: 0.85, ease: 'power3.out', delay: 0.15
    });

    // ── RSVP ──────────────────────────────────────────────────────────────────
    gsap.from('#rsvp .glass.rect', {
      scrollTrigger: { trigger: '#rsvp', start: 'top 72%' },
      opacity: 0, scale: 0.94, duration: 0.85, ease: 'power2.out'
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

    // ── Mesa de Regalos ───────────────────────────────────────────────────────
    gsap.from('#mesa-regalos .registry-card', {
      scrollTrigger: { trigger: '#mesa-regalos', start: 'top 72%' },
      opacity: 0, x: 58, duration: 0.75, stagger: 0.18, ease: 'power3.out'
    });

    // ── Video Mensaje ─────────────────────────────────────────────────────────
    gsap.from('#video-mensaje .label', {
      scrollTrigger: { trigger: '#video-mensaje', start: 'top 78%' },
      opacity: 0, y: -22, duration: 0.6, ease: 'power2.out'
    });
    gsap.from('#video-mensaje .glass.rect', {
      scrollTrigger: { trigger: '#video-mensaje', start: 'top 72%' },
      opacity: 0, y: 44, duration: 0.9, ease: 'power3.out', delay: 0.1
    });

    // ── Closing ───────────────────────────────────────────────────────────────
    gsap.from('.closing-line', {
      scrollTrigger: { trigger: '#closing', start: 'top 80%' },
      opacity: 0, y: 52, duration: 1.0, stagger: 0.28, ease: 'power3.out'
    });
  }

  // ── Parallax ──────────────────────────────────────────────────────────────

  function initParallax() {
    if (window.matchMedia('(max-width: 768px)').matches) return;
    document.querySelectorAll('.section').forEach(function (section) {
      gsap.to(section, {
        backgroundPositionY: '+=14%',
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

  // ── Back to top ───────────────────────────────────────────────────────────

  function initBackToTop() {
    var btn = document.getElementById('back-to-top');
    if (!btn) return;

    var visible = false;

    // Show after scrolling past hero height
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

    // Click: GSAP smooth scroll to top with luxurious ease
    btn.addEventListener('click', function () {
      // Brief button animation before scrolling
      gsap.to(btn, {
        scale: 0.88, duration: 0.12, ease: 'power2.in',
        onComplete: function () {
          gsap.to(btn, { scale: 1, duration: 0.2, ease: 'back.out(2)' });
        }
      });
      // Smooth scroll — override native scroll-behavior for GSAP control
      gsap.to(window, {
        scrollTo: { y: 0, autoKill: false },
        duration: 0.85,
        ease: 'power4.inOut'
      });
    });
  }

  // ── RSVP multi-step overlay ───────────────────────────────────────────────

  function initRSVP() {
    var overlay  = document.getElementById('rsvp-overlay');
    var openBtn  = document.getElementById('rsvp-open-btn');
    var closeBtn = document.getElementById('rsvp-close');
    if (!overlay || !openBtn) return;

    var guestCount = 1;

    // Open
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

    // Close
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

    // Step transitions
    function showStep(n) {
      var steps = document.querySelectorAll('.rsvp-step');
      steps.forEach(function (s) { s.style.display = 'none'; });
      var target = document.getElementById('rsvp-step-' + n);
      if (!target) return;
      target.style.display = 'block';
      gsap.from(target, {
        opacity: 0, x: 28, duration: 0.32, ease: 'power2.out'
      });
    }

    document.querySelectorAll('.rsvp-next').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var next = parseInt(btn.getAttribute('data-next'), 10);
        if (next === 4) buildSummary();
        showStep(next);
      });
    });

    // Counter
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

    // Build WhatsApp message from collected inputs
    function buildSummary() {
      var nameEl    = document.getElementById('rsvp-name');
      var dietaryEl = document.getElementById('rsvp-dietary');
      var name    = nameEl    ? nameEl.value.trim()    : '';
      var dietary = dietaryEl ? dietaryEl.value.trim() : '';

      var summaryEl = document.getElementById('rsvp-summary');
      if (summaryEl) {
        summaryEl.innerHTML =
          '<p>Nombre: <strong>' + escHtml(name || '—') + '</strong></p>' +
          '<p>Asistentes: <strong>' + guestCount + '</strong></p>' +
          (dietary
            ? '<p>Restricciones: <strong>' + escHtml(dietary) + '</strong></p>'
            : '');
      }

      var msg = '¡Hola! Confirmo mi asistencia a la boda de Daniel y Ana Cristina 🎉\n' +
        'Nombre: ' + (name || '—') + '\n' +
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

  // ── Mesa de Regalos — copy to clipboard ───────────────────────────────────

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
              btn.textContent = '¡Copiado!';
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
          // Clipboard API not available — silent fallback
        });
      });
    });
  }

})();
