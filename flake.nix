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

      flake.nixosModules = {
        database = ./nixosModules/database.nix;
        backend  = ./nixosModules/backend.nix;
        frontend = ./nixosModules/frontend.nix;
        wedding  = import ./nixosModules/wedding.nix;
        default  = { imports = [
          ./nixosModules/database.nix
          ./nixosModules/backend.nix
          ./nixosModules/frontend.nix
        ]; };
      };

      perSystem = { self', system, ... }:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        # Import reflex-platform for its GHCJS package set
        rp = import inputs.reflex-platform { inherit system; };

        # Native GHC build for the backend.  Extends nixpkgs.haskellPackages
        # with the local wedding-shared and wedding-backend packages via
        # callCabal2nix.  Stays separate from reflex-platform's GHCJS set.
        hpkgs = pkgs.haskellPackages.extend (self: super: {
          wedding-shared  = self.callCabal2nix "wedding-shared"  ./shared  {};
          wedding-backend = self.callCabal2nix "wedding-backend" ./backend {};
        });
        weddingBackend = pkgs.haskell.lib.justStaticExecutables hpkgs.wedding-backend;

        project = rp.project ({ ... }: {
          packages = {
            wedding-frontend = ./frontend;
            wedding-shared   = ./shared;
          };
          shells = {
            ghc   = [ "wedding-frontend" "wedding-shared" ];
            ghcjs = [ "wedding-frontend" "wedding-shared" ];
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

        # Text-free couple photos + dress-code cutouts (1.png–7.png)
        imagesAssets = builtins.path {
          name = "wedding-images";
          path = ./images;
          filter = path: type:
            type == "directory" ||
            (type == "regular" && builtins.baseNameOf path != ".gitkeep");
        };

        # Static website: index.html + GHCJS JS files + public assets
        website = pkgs.runCommand "wedding-website" {
          nativeBuildInputs = [ pkgs.rsync ];
        } ''
          mkdir -p "$out/images"

          # GHCJS compiled output first (rts.js lib.js out.js runmain.js …)
          rsync -r --no-perms --chmod=Du+rwx,Fu+rw \
            ${ghcjsBuild}/bin/wedding-frontend.jsexe/ "$out/"

          # Static public assets — rsync --no-perms keeps $out writable
          rsync -r --no-perms --chmod=Du+rwx,Fu+rw ${publicAssets}/ "$out/"

          # Text-free photos + dress-code cutouts
          rsync -r --no-perms --chmod=Du+rwx,Fu+rw ${imagesAssets}/ "$out/images/"

          # Our HTML shell overwrites any index.html from the jsexe bundle
          install -m644 ${./index.html} "$out/index.html"
        '';
      in {
        # ── Packages ────────────────────────────────────────────────────────
        packages.website         = website;
        packages.wedding-backend = weddingBackend;
        packages.default         = website;

        # ── Dev shell (GHC + cabal, jsaddle-warp browser preview) ──────────
        devShells.default = project.shells.ghc;

        # ── Apps ────────────────────────────────────────────────────────────

        # `nix run` → build static site and serve it locally with darkhttpd
        apps.default = {
          type    = "app";
          program = toString (pkgs.writeShellScript "serve-wedding" ''
            # Kill any previous darkhttpd holding port 3030
            ${pkgs.psmisc}/bin/fuser -k 3030/tcp 2>/dev/null || true
            echo "Wedding website served at http://localhost:3030"
            exec ${pkgs.darkhttpd}/bin/darkhttpd ${self'.packages.website} --port 3030
          '');
        };

        # `nix run .#build-site` → sync built static site into ./site
        apps.build-site = {
          type    = "app";
          program = toString (pkgs.writeShellScript "build-site" ''
            set -euo pipefail

            target_dir="$PWD/site"
            mkdir -p "$target_dir"

            ${pkgs.rsync}/bin/rsync -r --delete --no-perms --chmod=Du+rwx,Fu+rw \
              ${self'.packages.website}/ "$target_dir/"

            echo "Local deployable site refreshed in: $target_dir"
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

        # Ensures all required files are present in the built website
        checks.website-contents = pkgs.runCommand "check-website-contents" {} ''
          set -e
          test -f ${self'.packages.website}/index.html  || (echo "MISSING index.html"; exit 1)
          test -f ${self'.packages.website}/rts.js      || (echo "MISSING rts.js"; exit 1)
          test -f ${self'.packages.website}/out.js      || (echo "MISSING out.js"; exit 1)
          test -f ${self'.packages.website}/lib.js      || (echo "MISSING lib.js"; exit 1)
          test -d ${self'.packages.website}/images      || (echo "MISSING images/"; exit 1)
          for img in 1.png 2.png 3.png 4.png 5.png; do
            test -f ${self'.packages.website}/images/$img \
              || (echo "MISSING images/$img"; exit 1)
          done
          mkdir -p "$out"
        '';
      };
    };
}
