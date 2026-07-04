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
    agenix.url = "github:ryantm/agenix";

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
        nginx    = ./nixosModules/nginx.nix;
        # Legacy positional factory — kept until all consumers move to the
        # profile module below.
        wedding  = import ./nixosModules/wedding.nix;
        # Canonical interface: services.wedding.profile.* with packages and
        # secret paths defaulting to this flake's own outputs.
        default  = import ./nixosModules/profile.nix inputs.self;
      };

      perSystem = { self', system, inputs', ... }:
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
        weddingBackendBins = pkgs.haskell.lib.justStaticExecutables hpkgs.wedding-backend;
        weddingBackend = pkgs.runCommand "wedding-backend" { } ''
          mkdir -p "$out/bin" "$out/share/wedding-migrations"
          cp -r ${weddingBackendBins}/bin/. "$out/bin/"
          cp ${./backend/migrations}/*.sql "$out/share/wedding-migrations/"
        '';

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

        # GHCJS-compiled Haskell → .jsexe bundles
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
          nativeBuildInputs = [ pkgs.rsync pkgs.qrencode ];
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

          qrencode -t PNG -s 6 -m 2 -o "$out/qr-registry.png" \
            "https://mesaderegalos.liverpool.com.mx/milistaderegalos/51981423"
          qrencode -t PNG -s 6 -m 2 -o "$out/qr-location.png" \
            "https://www.google.com/maps/search/?api=1&query=20.5229282,-100.4039031"
        '';

        adminWebsite = pkgs.runCommand "wedding-admin-website" {
          nativeBuildInputs = [ pkgs.rsync ];
        } ''
          rsync -r --no-perms --chmod=Du+rwx,Fu+rw \
            ${ghcjsBuild}/bin/wedding-admin-frontend.jsexe/ "$out/"

          install -m644 ${./admin-index.html} "$out/index.html"
        '';
      in {
        # ── Packages ────────────────────────────────────────────────────────
        packages.website         = website;
        packages.admin-website   = adminWebsite;
        packages.wedding-backend = weddingBackend;
        packages.default         = website;

        # ── Dev shell (GHC + cabal, jsaddle-warp browser preview) ──────────
        devShells.default = pkgs.mkShell {
          inputsFrom = [ project.shells.ghc ];
          packages = [ inputs'.agenix.packages.default ];
        };

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
        checks.admin-website = self'.packages.admin-website;

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
          test -f ${self'.packages.admin-website}/index.html  || (echo "MISSING admin index.html"; exit 1)
          test -f ${self'.packages.admin-website}/out.js      || (echo "MISSING admin out.js"; exit 1)
          test -d ${self'.packages.website}/images      || (echo "MISSING images/"; exit 1)
          for img in 1.png 2.png 3.png 4.png 5.png; do
            test -f ${self'.packages.website}/images/$img \
              || (echo "MISSING images/$img"; exit 1)
          done
          mkdir -p "$out"
        '';

        # Ensures the canonical profile module keeps its interface: correct
        # wiring of nginx/backend/database plus dev-mode conveniences. All
        # assertions are eval-time; production mode is exercised from the
        # consumer flake (it needs agenix).
        checks.wedding-profile-interface =
          let
            lib = inputs.nixpkgs.lib;
            sys = lib.nixosSystem {
              inherit system;
              modules = [
                inputs.self.nixosModules.default
                {
                  services.wedding.profile = {
                    enable = true;
                    mode = "development";
                    serverName = "wedding.local";
                    ports = { nginx = 8084; backend = 3001; };
                  };
                }
              ];
            };
            c = sys.config;
            vhost = c.services.nginx.virtualHosts."wedding.local";
            checkList = [
              { name = "api proxyPass";
                ok = vhost.locations."/api/".proxyPass == "http://127.0.0.1:3001"; }
              { name = "login rate limit";
                ok = lib.hasInfix "limit_req zone=wedding_login"
                       vhost.locations."= /api/admin/login".extraConfig; }
              { name = "limit_req zone declared";
                ok = lib.hasInfix "limit_req_zone" c.services.nginx.appendHttpConfig; }
              { name = "admin alias";
                ok = toString vhost.locations."/admin/".alias
                       == "${self'.packages.admin-website}/"; }
              { name = "admin redirect";
                ok = vhost.locations."= /admin".return == "302 /admin/"; }
              { name = "self-default backend package";
                ok = c.services.wedding.backend.package == self'.packages.wedding-backend; }
              { name = "self-default static root";
                ok = toString vhost.root == toString self'.packages.website; }
              { name = "security headers";
                ok = lib.hasInfix "X-Content-Type-Options" vhost.extraConfig; }
              { name = "no HSTS in development";
                ok = !(lib.hasInfix "Strict-Transport-Security" vhost.extraConfig); }
              { name = "dev hosts alias";
                ok = lib.elem "wedding.local" c.networking.hosts."127.0.0.1"; }
              { name = "dev postgres trust auth";
                ok = lib.hasInfix "host all wedding 127.0.0.1/32 trust"
                       c.services.postgresql.authentication; }
              { name = "dev cookie not secure";
                ok = c.systemd.services.wedding-backend.environment.WEDDING_COOKIE_SECURE == "false"; }
            ];
            failed = builtins.filter (x: !x.ok) checkList;
          in
            if failed == [ ]
            then pkgs.runCommand "check-wedding-profile-interface" {} "mkdir -p $out"
            else throw "wedding-profile-interface failed: ${
              lib.concatMapStringsSep ", " (x: x.name) failed}";

        # Ensures the reusable NixOS module interface stays in sync with the
        # package set it expects from downstream flakes.
        checks.wedding-module-interface =
          let
            cfg = inputs.nixpkgs.lib.nixosSystem {
              inherit system;
              modules = [
                (import ./nixosModules/wedding.nix {
                  ports = { nginx = 8084; backend = 3001; database = 5432; };
                  databaseName = "wedding-test";
                  serverName = "wedding.local";
                  packages = {
                    backend = self'.packages.wedding-backend;
                    staticRoot = self'.packages.website;
                    adminStaticRoot = self'.packages.admin-website;
                  };
                  localPostgresTrust = true;
                  recommendedGzipSettings = false;
                })
              ];
            };
          in pkgs.runCommand "check-wedding-module-interface" {} ''
            test "${toString cfg.config.services.wedding.frontend.staticRoot}" = "${self'.packages.website}"
            test "${toString cfg.config.services.wedding.frontend.adminStaticRoot}" = "${self'.packages.admin-website}"
            test "${cfg.config.services.nginx.virtualHosts."wedding.local".locations."/api/".proxyPass}" = "http://127.0.0.1:3001"
            test "${toString cfg.config.services.nginx.virtualHosts."wedding.local".locations."/admin/".alias}" = "${self'.packages.admin-website}/"
            test "${cfg.config.services.nginx.virtualHosts."wedding.local".locations."= /admin".return}" = "302 /admin/"
            mkdir -p "$out"
          '';
      };
    };
}
