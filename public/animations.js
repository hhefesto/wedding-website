(function () {
  'use strict';

  // Wait until Reflex-DOM has rendered the full page tree.
  function waitForReflex() {
    return new Promise(function (resolve) {
      if (document.querySelector('#mesa-regalos')) {
        resolve();
        return;
      }
      var obs = new MutationObserver(function (_, observer) {
        if (document.querySelector('#mesa-regalos')) {
          observer.disconnect();
          resolve();
        }
      });
      obs.observe(document.body || document.documentElement, {
        childList: true,
        subtree: true
      });
    });
  }

  waitForReflex().then(initAll);

  function initAll() {
    gsap.registerPlugin(ScrollTrigger, ScrollToPlugin);

    var prefersReduced = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    initBackToTop();
    initCollageActiveState();

    if (prefersReduced) {
      revealStaticFallback();
      return;
    }

    initProgressBar();
    initCollageEntrance();
    initZoomSections();
    ScrollTrigger.refresh();
  }

  // ── Static fallback for prefers-reduced-motion ──────────────────────────────

  function revealStaticFallback() {
    document.querySelectorAll('[data-reveal]').forEach(function (el) {
      el.style.opacity = '1';
      el.style.transform = 'none';
    });
    document.querySelectorAll('.zoom-section .section-photo').forEach(function (el) {
      el.style.clipPath = 'none';
      el.style.transform = 'none';
    });
  }

  // ── Progress bar (GSAP ScrollTrigger scrub) ─────────────────────────────────

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

  // ── Collage entrance (scroll-triggered stagger) ─────────────────────────────

  function initCollageEntrance() {
    var cards = document.querySelectorAll('.collage-card');
    if (!cards.length) return;

    gsap.from('.collage-heading > *', {
      opacity: 0,
      y: 22,
      duration: 0.72,
      stagger: 0.08,
      ease: 'power2.out',
      scrollTrigger: {
        trigger: '#collage',
        start: 'top 74%',
        once: true
      }
    });

    gsap.from(cards, {
      opacity: 0,
      y: 26,
      scale: 0.9,
      duration: 0.75,
      stagger: 0.09,
      ease: 'power3.out',
      delay: 0.1,
      scrollTrigger: {
        trigger: '#collage',
        start: 'top 74%',
        once: true
      }
    });
  }

  // ── Zoom sections (pinned clip-path reveal on scroll) ────────────────────────

  function initZoomSections() {
    var sections = document.querySelectorAll('.zoom-section');
    if (!sections.length) return;

    sections.forEach(function (section) {
      var photo = section.querySelector('.section-photo');
      if (!photo) return;

      var reveals = section.querySelectorAll('[data-reveal]');
      var origin = section.getAttribute('data-zoom-origin') || '50% 50%';

      gsap.set(photo, {
        transformOrigin: origin,
        scale: 1.22,
        clipPath: 'inset(18% 10% 18% 10% round 22px)'
      });

      gsap.set(reveals, { autoAlpha: 0, y: 28 });

      var tl = gsap.timeline({
        scrollTrigger: {
          trigger: section,
          start: 'top top',
          end: '+=130%',
          pin: true,
          scrub: 0.55,
          invalidateOnRefresh: true,
          anticipatePin: 1
        }
      });

      tl.to(photo, {
        scale: 1,
        clipPath: 'inset(0% 0% 0% 0% round 0px)',
        ease: 'none'
      }, 0)
        .to(reveals, {
          autoAlpha: 1,
          y: 0,
          stagger: 0.08,
          duration: 0.4,
          ease: 'power2.out'
        }, 0.3);
    });
  }

  // ── Collage active-state (highlight card when its section is in view) ────────
  // Smooth scrolling is handled by CSS `scroll-behavior: smooth` on html.

  function initCollageActiveState() {
    var cards = document.querySelectorAll('.collage-card');
    if (!cards.length) return;

    document.querySelectorAll('.zoom-section').forEach(function (section) {
      var id = section.id;
      if (!id) return;

      ScrollTrigger.create({
        trigger: section,
        start: 'top center',
        end: 'bottom center',
        onToggle: function (self) {
          var match = document.querySelector('.collage-card[href="#' + id + '"]');
          if (!match) return;
          match.classList.toggle('is-active', self.isActive);
        }
      });
    });
  }

  // ── Back to top ─────────────────────────────────────────────────────────────

  function initBackToTop() {
    var btn = document.getElementById('back-to-top');
    if (!btn) return;

    function onScroll() {
      var shouldShow = window.scrollY > window.innerHeight * 0.9;
      if (shouldShow && btn.style.display !== 'block') {
        btn.style.display = 'block';
        gsap.fromTo(btn,
          { opacity: 0, scale: 0.72, y: 16 },
          { opacity: 1, scale: 1, y: 0, duration: 0.28, ease: 'back.out(1.8)' }
        );
      }
      if (!shouldShow && btn.style.display === 'block') {
        gsap.to(btn, {
          opacity: 0,
          scale: 0.72,
          y: 16,
          duration: 0.25,
          ease: 'power2.in',
          onComplete: function () { btn.style.display = 'none'; }
        });
      }
    }

    window.addEventListener('scroll', onScroll, { passive: true });
    onScroll();

    btn.addEventListener('click', function () {
      gsap.to(window, {
        scrollTo: { y: 0, autoKill: false },
        duration: 0.85,
        ease: 'power4.inOut'
      });
    });
  }

})();;
