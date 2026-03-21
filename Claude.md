# CLAUDE.md — Frontend Website Rules

## Always Do First
- **Invoke the `frontend-design` skill** before writing any frontend code, every session, no exceptions.

## Reference Images
- If a reference image is provided: match layout, spacing, typography, and color exactly. Swap in placeholder content (images via `https://placehold.co/`, generic copy). Do not improve or add to the design.
- If no reference image: design from scratch with high craft (see guardrails below).
- Screenshot your output, compare against reference, fix mismatches, re-screenshot. Do at least 2 comparison rounds. Stop only when no visible differences remain or user says so.

## Local Server

- Enter the dev shell first if not already in it: `nix develop`
- Build and serve: `spago build && spago bundle --outfile public/index.js && darkhttpd public --port 3000`
- For watch mode during development run: `nix run` builds the full site and serves it at `http://localhost:3000`.
- If the server is already running, do not start a second instance.

## Output Defaults
- UI is written in **PureScript** (targeting the browser), compiled via `spago build && spago bundle` with `esbuild`.
- Entry point: `src/Main.purs`. Output bundle: `index.js`. Static shell: `public/index.html`.
- Prefer **Halogen** for UI components; add it to `spago.yaml` dependencies if not present.
- CSS: inline `<style>` in `public/index.html` or a companion `public/style.css` — no CDN JS frameworks.
- Placeholder images: `https://placehold.co/WIDTHxHEIGHT`
- Mobile-first responsive

## Anti-Generic Guardrails
- **Colors:** Never use default Tailwind palette (indigo-500, blue-600, etc.). Pick a custom brand color and derive from it.
- **Shadows:** Never use flat `shadow-md`. Use layered, color-tinted shadows with low opacity.
- **Typography:** Never use the same font for headings and body. Pair a display/serif with a clean sans. Apply tight tracking (`-0.03em`) on large headings, generous line-height (`1.7`) on body.
- **Gradients:** Layer multiple radial gradients. Add grain/texture via SVG noise filter for depth.
- **Animations:** Only animate `transform` and `opacity`. Never `transition-all`. Use spring-style easing.
- **Interactive states:** Every clickable element needs hover, focus-visible, and active states. No exceptions.
- **Images:** Add a gradient overlay (`bg-gradient-to-t from-black/60`) and a color treatment layer with `mix-blend-multiply`.
- **Spacing:** Use intentional, consistent spacing tokens — not random Tailwind steps.
- **Depth:** Surfaces should have a layering system (base → elevated → floating), not all sit at the same z-plane.

## Hard Rules
- Do not add sections, features, or content not in the reference
- Do not "improve" a reference design — match it
- Do not stop after one screenshot pass
- Do not use `transition-all`
- Do not use default Tailwind blue/indigo as primary color
- **Write UI logic in PureScript — never in raw JavaScript**
- **No `node`/npm tooling** — use `spago`, `purs`, `esbuild`, and `nix` exclusively
- **No CDN JS frameworks** (React, Vue, Alpine, etc.) — use Halogen or Halogen-compatible PureScript libs
