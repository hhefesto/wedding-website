{
  nixConfig = {
    allow-import-from-derivation = true;
    extra-substituters          = [ "https://nixcache.reflex-frp.org" ];
    extra-trusted-public-keys   = [
      "ryantrinkle.com-1:JJiAKaRv9mwhkerZRpQmMkMsk+p2JXCetKFVJFgZB6Y="
    ];
  };

  inputs = {
    nixpkgs.url    = "github:NixOS/nixpkgs/nixos-24.05";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # reflex-platform provides a ready-made GHCJS + reflex-dom package set
    reflex-platform = {
      url   = "github:reflex-frp/reflex-platform";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { self', system, ... }:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        # Import reflex-platform for its GHCJS package set
        rp = import inputs.reflex-platform { inherit system; };

        project = rp.project ({ ... }: {
          packages = {
            wedding-frontend = ./.;
          };
          shells = {
            ghc   = [ "wedding-frontend" ];
            ghcjs = [ "wedding-frontend" ];
          };
        });

        # GHCJS-compiled Haskell → .jsexe bundle
        ghcjsBuild = project.ghcjs.wedding-frontend;

        # Gather public/ assets into the nix store.
        # We use builtins.path so the derivation does not fail when the
        # directory contains only empty subdirs (e.g. images/ before photos
        # are added).  The filter keeps real files and skips .gitkeep.
        publicAssets = builtins.path {
          name = "wedding-public";
          path = ./public;
          filter = path: type:
            type == "directory" ||
            (type == "regular" && builtins.baseNameOf path != ".gitkeep");
        };

        # Static website: index.html + GHCJS JS files + public assets
        website = pkgs.runCommand "wedding-website" {
          nativeBuildInputs = [ pkgs.rsync ];
        } ''
          mkdir -p "$out/images"

          # Static assets (images etc.) — rsync --no-perms keeps $out writable
          rsync -r --no-perms --chmod=Du+rwx,Fu+rw ${publicAssets}/ "$out/"

          # HTML shell
          install -m644 ${./index.html} "$out/index.html"

          # GHCJS compiled output (rts.js lib.js out.js runmain.js …)
          rsync -r --no-perms --chmod=Du+rwx,Fu+rw \
            ${ghcjsBuild}/bin/wedding-frontend.jsexe/ "$out/"
        '';
      in {
        # ── Packages ────────────────────────────────────────────────────────
        packages.website = website;
        packages.default = website;

        # ── Dev shell (GHC + cabal, jsaddle-warp browser preview) ──────────
        devShells.default = project.shells.ghc;

        # ── Apps ────────────────────────────────────────────────────────────

        # `nix run` → build static site and serve it locally with darkhttpd
        apps.default = {
          type    = "app";
          program = toString (pkgs.writeShellScript "serve-wedding" ''
            echo "Wedding website served at http://localhost:3000"
            exec ${pkgs.darkhttpd}/bin/darkhttpd ${self'.packages.website} --port 3000
          '');
        };

        # ── Checks ──────────────────────────────────────────────────────────

        # Ensures the GHCJS static build succeeds
        checks.website = self'.packages.website;

        # Ensures darkhttpd + the website package are both available
        checks.default-app = pkgs.runCommand "check-default-app" {} ''
          test -f ${pkgs.darkhttpd}/bin/darkhttpd
          test -d ${self'.packages.website}
          mkdir -p "$out"
        '';
      };
    };
}
