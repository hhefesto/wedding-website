# Wedding Website: GSAP Animations, WhatsApp RSVP, Video Messages & Gift Table

## Context

The wedding website for Daniel y Ana Cristina (10/10/26) is a single-page Haskell/Reflex-DOM/GHCJS static site with 6 full-viewport sections. It currently has **zero animations and minimal interactivity**. The user wants:
- GSAP animations for scroll reveals, parallax, and micro-interactions
- RSVP via WhatsApp with an animated in-page interface
- Video message section via WhatsApp deep link
- Enhanced Mesa de Regalos with multiple registries + copy-to-clipboard

Prototype images are in `./prototype/` (1.png–5.png) — beautiful forest/outdoor couple photos with warm tones, glassmorphism cards, Great Vibes cursive + Courier Prime mono typography.

---

## Decision: GSAP (not Three.js)

Three.js is 3D/WebGL — overkill here. GSAP is purpose-built for scroll-triggered 2D reveals, parallax, staggered fades. ~70KB (core + ScrollTrigger).

---

## Files Modified

| File | Action |
|------|--------|
| `src/Main.hs` | RSVP WhatsApp form UI, video message section, enhanced mesa de regalos, CSS for animations + new features |
| `index.html` | GSAP CDN scripts + `animations.js` |
| `public/animations.js` | **NEW** — all GSAP animation logic + clipboard + WhatsApp link helpers |
| `flake.nix` | `animations.js` added to content checks |

---

## Phase 1: GSAP Foundation

### `index.html`
- GSAP core + ScrollTrigger via CDN in `<head>`
- `<script src="animations.js" defer></script>` after `runmain.js`

### `public/animations.js` — MutationObserver bootstrap
Since Reflex builds DOM dynamically, uses a `MutationObserver` watching for `#closing` (last section) to appear, then inits all animations and disconnects.

---

## Phase 2: Section Animations

| Section | Animation |
|---------|-----------|
| **Hero** | Names stagger fade-in + slide up (0.8s, 0.3s stagger). Date fades after. Nav links slide up with stagger. Background parallax on scroll-out. |
| **Ubicacion** | `.glass.blob` scales 0.92→1.0 + fade on scroll enter. Slow breathing tween on `border-radius` (8s loop). |
| **Dress Code** | Label fades from top. Glass card slides in from left. |
| **RSVP** | Card scales 0.95→1.0 + fade. Button pulse glow on enter. |
| **Mesa de Regalos** | Cards stagger in from right. Copy button GSAP feedback. |
| **Video Mensaje** | Card fades in + slides up. |
| **Closing** | Text lines stagger in from bottom-left. |
| **Global** | Subtle parallax on all background images (~15%). Disabled on mobile. |

---

## Phase 3: RSVP via WhatsApp (Animated Multi-Step)

1. Guest taps "Confirmar" → glass overlay animates in (GSAP)
2. **Step 1:** "¿Cómo te llamas?" — name input
3. **Step 2:** "¿Cuántos asistirán?" — animated +/- counter
4. **Step 3:** "¿Alguna restricción alimentaria?" — optional text
5. **Step 4:** Summary + "Enviar por WhatsApp →" button (builds `wa.me/PLACEHOLDER?text=...`)

Logic entirely in `animations.js` — Haskell renders the static HTML structure, JS handles step transitions, data collection, and URL construction.

---

## Phase 4: Video Message Section

New section `#video-mensaje` between Mesa de Regalos and Closing:
- Label "VIDEO PARA LOS NOVIOS"
- Glass card with text + WhatsApp deep link button
- Background: warm color fallback `#382e24` (user to provide image later)
- Nav link added to hero strip

---

## Phase 5: Enhanced Mesa de Regalos

- Liverpool card: number + copy-to-clipboard button
- Placeholder card for future registry
- GSAP copy feedback: "Copiar" → "¡Copiado!" with fade

---

## Phase 6: Build & Polish

- `flake.nix`: `animations.js` added to `checks.website-contents`
- `nix flake check` verified passing

---

## Resolved Decisions

- **WhatsApp number:** `PLACEHOLDER` — swap later
- **Additional registries:** Multi-card structure built, user fills in later
- **Video section background:** Solid `#382e24` fallback — user to provide image later
