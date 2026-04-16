(function () {
  'use strict';

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

    initMagnetic();
    initBackToTop();
    initRSVP();
    initMesaRegalos();
    initMarquee();
    initCollageLinks();

    if (prefersReduced) {
      var introReduced = document.getElementById('intro');
      if (introReduced) {
        introReduced.style.display = 'none';
      }
      revealStaticFallback();
      return;
    }

    initProgressBar();
    initIntro();
  }

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

  function initProgressBar() {
    var bar = document.getElementById('progress-bar');
    if (!bar) {
      return;
    }
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

  function splitWords(selector, mode) {
    mode = mode || 'word';

    document.querySelectorAll(selector).forEach(function (el) {
      var text = el.textContent || '';
      el.textContent = '';

      var units = mode === 'char' ? text.split('') : text.split(/(\s+)/);
      units.forEach(function (unit) {
        if (/^\s+$/.test(unit)) {
          el.appendChild(document.createTextNode('\u00a0'));
          return;
        }

        var outer = document.createElement('span');
        outer.className = 'sw';
        var inner = document.createElement('span');
        inner.className = 'sw-i';
        inner.textContent = unit;
        outer.appendChild(inner);
        el.appendChild(outer);
      });
    });
  }

  function initIntro() {
    var introEl = document.getElementById('intro');
    if (!introEl) {
      initStoryboard();
      return;
    }

    splitWords('.intro-kicker', 'word');

    gsap.set('.intro-kicker .sw-i', { y: '110%' });
    gsap.set('.intro-rule', { width: 0 });
    gsap.set('.intro-sign', { opacity: 0, y: 22 });
    gsap.set('#intro', { clipPath: 'inset(0% 0% 0% 0%)' });

    var tl = gsap.timeline({
      onComplete: function () {
        initStoryboard();
      }
    });

    tl.to('.intro-kicker .sw-i', {
      y: '0%',
      duration: 0.74,
      stagger: 0.07,
      ease: 'power3.out'
    })
      .to('.intro-rule', {
        width: '60vw',
        duration: 0.56,
        ease: 'power2.out'
      }, '-=0.28')
      .to('.intro-sign', {
        opacity: 1,
        y: 0,
        duration: 0.7,
        ease: 'power3.out'
      }, '-=0.18')
      .to({}, { duration: 0.72 })
      .to('.intro-inner', {
        opacity: 0,
        y: -20,
        duration: 0.42,
        ease: 'power2.in'
      })
      .to('#intro', {
        clipPath: 'inset(50% 0% 50% 0%)',
        duration: 0.9,
        ease: 'power4.inOut'
      }, '-=0.05')
      .set('#intro', { display: 'none' });

    introEl.addEventListener('click', function () {
      tl.progress(0.88);
    }, { once: true });
  }

  function initStoryboard() {
    initCollageEntrance();
    initZoomSections();
    ScrollTrigger.refresh();
  }

  function initCollageEntrance() {
    var cards = document.querySelectorAll('.collage-card');
    if (!cards.length) {
      return;
    }

    gsap.from('.collage-heading > *', {
      opacity: 0,
      y: 22,
      duration: 0.72,
      stagger: 0.08,
      ease: 'power2.out'
    });

    gsap.from(cards, {
      opacity: 0,
      y: 26,
      scale: 0.9,
      duration: 0.75,
      stagger: 0.09,
      ease: 'power3.out',
      delay: 0.12
    });
  }

  function initCollageLinks() {
    var cards = Array.prototype.slice.call(document.querySelectorAll('.collage-card'));
    if (!cards.length) {
      return;
    }

    cards.forEach(function (card) {
      card.addEventListener('click', function (event) {
        var href = card.getAttribute('href');
        if (!href || href.charAt(0) !== '#') {
          return;
        }

        var target = document.querySelector(href);
        if (!target) {
          return;
        }

        event.preventDefault();
        gsap.to(window, {
          scrollTo: { y: target, autoKill: false },
          duration: 0.9,
          ease: 'power4.inOut'
        });
      });
    });

    document.querySelectorAll('.zoom-section').forEach(function (section) {
      var id = section.id;
      if (!id) {
        return;
      }

      ScrollTrigger.create({
        trigger: section,
        start: 'top center',
        end: 'bottom center',
        onToggle: function (self) {
          var match = document.querySelector('.collage-card[href="#' + id + '"]');
          if (!match) {
            return;
          }
          match.classList.toggle('is-active', self.isActive);
        }
      });
    });
  }

  function initZoomSections() {
    var sections = document.querySelectorAll('.zoom-section');
    if (!sections.length) {
      return;
    }

    sections.forEach(function (section) {
      var photo = section.querySelector('.section-photo');
      if (!photo) {
        return;
      }

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

  function initMarquee() {
    var track = document.querySelector('.marquee-track');
    if (!track) {
      return;
    }

    gsap.to(track, {
      xPercent: -50,
      duration: 30,
      ease: 'none',
      repeat: -1
    });
  }

  function initMagnetic() {
    document.querySelectorAll('[data-magnetic]').forEach(function (el) {
      el.addEventListener('mousemove', function (e) {
        var rect = el.getBoundingClientRect();
        var dx = e.clientX - (rect.left + rect.width / 2);
        var dy = e.clientY - (rect.top + rect.height / 2);

        gsap.to(el, {
          x: dx * 0.28,
          y: dy * 0.28,
          duration: 0.35,
          ease: 'power2.out'
        });
      });

      el.addEventListener('mouseleave', function () {
        gsap.to(el, {
          x: 0,
          y: 0,
          duration: 0.55,
          ease: 'elastic.out(1, 0.45)'
        });
      });
    });
  }

  function initBackToTop() {
    var btn = document.getElementById('back-to-top');
    if (!btn) {
      return;
    }

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
          onComplete: function () {
            btn.style.display = 'none';
          }
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

  function initRSVP() {
    var overlay = document.getElementById('rsvp-overlay');
    var openBtn = document.getElementById('rsvp-open-btn');
    var closeBtn = document.getElementById('rsvp-close');
    if (!overlay || !openBtn) {
      return;
    }

    var guestCount = 1;

    openBtn.addEventListener('click', function () {
      overlay.style.display = 'flex';

      gsap.fromTo(overlay,
        { opacity: 0 },
        { opacity: 1, duration: 0.32, ease: 'power2.out' }
      );

      gsap.fromTo('.rsvp-modal',
        { scale: 0.9, opacity: 0, y: 24 },
        { scale: 1, opacity: 1, y: 0, duration: 0.38, ease: 'back.out(1.6)' }
      );

      showStep(1);
    });

    function closeOverlay() {
      gsap.to('.rsvp-modal', {
        scale: 0.92,
        opacity: 0,
        y: 14,
        duration: 0.2,
        ease: 'power2.in'
      });

      gsap.to(overlay, {
        opacity: 0,
        duration: 0.24,
        delay: 0.06,
        ease: 'power2.in',
        onComplete: function () {
          overlay.style.display = 'none';
          guestCount = 1;
          showStep(1);
          updateCount();
        }
      });
    }

    if (closeBtn) {
      closeBtn.addEventListener('click', closeOverlay);
    }

    overlay.addEventListener('click', function (event) {
      if (event.target === overlay) {
        closeOverlay();
      }
    });

    function showStep(step) {
      var steps = document.querySelectorAll('.rsvp-step');
      steps.forEach(function (el) { el.style.display = 'none'; });

      var target = document.getElementById('rsvp-step-' + step);
      if (!target) {
        return;
      }
      target.style.display = 'block';

      gsap.from(target, {
        opacity: 0,
        x: 24,
        duration: 0.28,
        ease: 'power2.out'
      });
    }

    document.querySelectorAll('.rsvp-next').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var next = parseInt(btn.getAttribute('data-next'), 10);
        if (next === 4) {
          buildSummary();
        }
        showStep(next);
      });
    });

    function updateCount() {
      var countEl = document.getElementById('rsvp-count');
      if (countEl) {
        countEl.textContent = String(guestCount);
      }
    }

    function animateCount() {
      gsap.from('#rsvp-count', {
        scale: 1.4,
        duration: 0.2,
        ease: 'back.out(2)'
      });
    }

    var minusBtn = document.getElementById('rsvp-minus');
    var plusBtn = document.getElementById('rsvp-plus');

    if (minusBtn) {
      minusBtn.addEventListener('click', function () {
        if (guestCount > 1) {
          guestCount -= 1;
          updateCount();
          animateCount();
        }
      });
    }

    if (plusBtn) {
      plusBtn.addEventListener('click', function () {
        if (guestCount < 10) {
          guestCount += 1;
          updateCount();
          animateCount();
        }
      });
    }

    function buildSummary() {
      var nameEl = document.getElementById('rsvp-name');
      var dietaryEl = document.getElementById('rsvp-dietary');

      var name = nameEl ? nameEl.value.trim() : '';
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

      var message =
        '\u00a1Hola! Confirmo mi asistencia a la boda de Daniel y Ana Cristina \ud83c\udf89\n' +
        'Nombre: ' + (name || '\u2014') + '\n' +
        'Asistentes: ' + guestCount +
        (dietary ? '\nRestricciones: ' + dietary : '');

      var waBtn = document.getElementById('rsvp-whatsapp-btn');
      if (waBtn) {
        waBtn.href = 'https://wa.me/PLACEHOLDER?text=' + encodeURIComponent(message);
      }
    }

    function escHtml(value) {
      return String(value)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
    }
  }

  function initMesaRegalos() {
    document.querySelectorAll('.copy-btn').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var val = btn.getAttribute('data-copy');
        if (!val) {
          return;
        }

        navigator.clipboard.writeText(val).then(function () {
          var original = btn.textContent;
          gsap.to(btn, {
            opacity: 0.55,
            duration: 0.1,
            onComplete: function () {
              btn.textContent = 'Copiado!';
              gsap.to(btn, {
                opacity: 1,
                duration: 0.12,
                onComplete: function () {
                  setTimeout(function () {
                    gsap.to(btn, {
                      opacity: 0.55,
                      duration: 0.1,
                      onComplete: function () {
                        btn.textContent = original;
                        gsap.to(btn, { opacity: 1, duration: 0.12 });
                      }
                    });
                  }, 1400);
                }
              });
            }
          });
        }).catch(function () {
          // Clipboard not available.
        });
      });
    });
  }

})();
