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
    initFixedNav();

    if (prefersReduced) {
      revealStaticFallback();
      return;
    }

    initProgressBar();
    initParallaxSections();
    ScrollTrigger.refresh();
  }

  // ── Static fallback for prefers-reduced-motion ──────────────────────────────

  function revealStaticFallback() {
    document.querySelectorAll('[data-reveal]').forEach(function (el) {
      el.style.opacity = '1';
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

  // ── Parallax image sections ──────────────────────────────────────────────────
  // Each .image-section: parallax drift on the photo + staggered content reveal
  // + soft crossfade-out as you scroll past.

  function initParallaxSections() {
    var sections = document.querySelectorAll('.image-section');
    if (!sections.length) return;

    sections.forEach(function (section) {
      var img = section.querySelector('.section-img');
      var reveals = section.querySelectorAll('[data-reveal]');

      // Parallax: photo drifts up at ~0.5x scroll speed (subtle, cinematic)
      if (img) {
        gsap.to(img, {
          yPercent: -10,
          ease: 'none',
          scrollTrigger: {
            trigger: section,
            start: 'top bottom',
            end: 'bottom top',
            scrub: 0.7,
            invalidateOnRefresh: true
          }
        });
      }

      // Staggered overlay content reveal on scroll into view
      if (reveals.length) {
        gsap.set(reveals, { autoAlpha: 0, y: 28 });
        gsap.to(reveals, {
          autoAlpha: 1,
          y: 0,
          stagger: 0.11,
          duration: 0.72,
          ease: 'power2.out',
          scrollTrigger: {
            trigger: section,
            start: 'top 62%',
            once: true
          }
        });
      }

      // Crossfade: section dims gently as you scroll past
      gsap.to(section, {
        opacity: 0.22,
        ease: 'none',
        scrollTrigger: {
          trigger: section,
          start: 'bottom 72%',
          end: 'bottom 10%',
          scrub: true
        }
      });
    });
  }

  // ── Fixed bottom nav: entrance + active section highlighting ─────────────────

  function initFixedNav() {
    var nav = document.getElementById('fixed-nav');
    if (!nav) return;

    // Slide up into view after the intro animation completes (~3.45s)
    gsap.to(nav, {
      opacity: 1,
      y: 0,
      duration: 0.6,
      ease: 'power2.out',
      delay: 3.6
    });

    // Highlight the nav link matching whichever section is centred in viewport
    var links = nav.querySelectorAll('.fixed-nav-link');
    document.querySelectorAll('.image-section').forEach(function (section) {
      ScrollTrigger.create({
        trigger: section,
        start: 'top center',
        end: 'bottom center',
        onToggle: function (self) {
          var id = section.id;
          links.forEach(function (link) {
            if (link.getAttribute('data-section') === id) {
              link.classList.toggle('is-active', self.isActive);
            }
          });
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

})();
