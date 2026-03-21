{
  description = "Wedding website (PureScript + Halogen, reproducible)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";

    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
  };

  outputs = inputs@{ nixpkgs, flake-parts, purescript-overlay, mkSpagoDerivation, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { system, self', ... }: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            mkSpagoDerivation.overlays.default
            purescript-overlay.overlays.default
          ];
        };
      in {
        _module.args.pkgs = pkgs;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            spago-unstable
            purs-unstable
            esbuild
            nodejs
            watchexec
            jq
            darkhttpd
            imagemagick
            scrot
            # nodePackages.purescript-language-server
          ];
          shellHook = ''
            echo "Wedding Website (PureScript + Halogen)"
            echo "• spago build        - typecheck/compile"
            echo "• spago bundle       - bundle to index.js"
            echo "• nix run            - build & serve at localhost:3000"
          '';
        };

        packages = {
          website = pkgs.mkSpagoDerivation {
            src = ./.;
            spagoYaml = ./spago.yaml;
            spagoLock = ./spago.lock;

            version = "0.1.0";
            nativeBuildInputs = with pkgs; [ spago-unstable purs-unstable esbuild ];

            buildPhase = ''
              set -euo pipefail
              export HOME="$PWD/.nix-build-home"
              spago bundle
            '';

            installPhase = ''
              set -euo pipefail
              mkdir -p "$out"
              if [ -d public ]; then
                cp -r public/* "$out"/
              else
                cat > "$out/index.html" <<'HTML'
              <!doctype html>
              <html lang="en">
              <head>
                <meta charset="utf-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <title>Our Wedding</title>
              </head>
              <body>
                <div id="root"></div>
                <script type="module" src="./index.js"></script>
              </body>
              </html>
              HTML
              fi
              cp index.js "$out"/
              [ -d assets ] && cp -r assets "$out"/
            '';
          };

          default = self'.packages.website;
        };

        apps.default = {
          type = "app";
          program = "${pkgs.writeShellScript "serve-wedding" ''
            ${pkgs.darkhttpd}/bin/darkhttpd ${self'.packages.website} --port 3000
          ''}";
        };

        checks = {
          typecheck = pkgs.runCommand "purescript-typecheck" {
            nativeBuildInputs = with pkgs; [ spago-unstable purs-unstable ];
          } ''
            set -euo pipefail
            cp -r ${./.} ./
            chmod -R +w .
            export HOME="$PWD/.nix-build-home"
            spago build
            touch "$out"
          '';
          bundle = self'.packages.website;
        };
      };
    };
}
