# Reusable wedding RSVP stack module.
#
# Usage (from a consumer flake):
#
#   wedding = (import "${inputs.wedding-page}/nixosModules/wedding.nix") {
#     ports        = { nginx = 8084; backend = 3001; database = 5432; };
#     packages = {
#       backend    = inputs.wedding-page.packages.${system}.wedding-backend;
#       staticRoot = inputs.wedding-page.packages.${system}.website;
#     };
#     databaseName = "wedding";
#     serverName   = "wedding.local";
#     localHostAlias = true;
#   };
#
# Then add `wedding` to the host's modules list.
# Postgres-sharing invariant: every project on the same host MUST agree on
# `ports.database` (NixOS cannot merge a single-valued port across modules).

{ ports                       # { nginx, backend, database }
, packages                    # { backend, staticRoot }
, databaseName ? "wedding"    # DB name and owning role on the shared cluster
, serverName   ? "_"
, openFirewall ? true
, databasePasswordFile ? null
, databaseUrlFile ? null
, adminPasswordHashFile ? null
, videoDir ? "/var/lib/wedding/videos"
, videoMaxBytes ? 200 * 1024 * 1024
, cookieSecure ? false
, uploadMaxBodySize ? "200m"
, localHostAlias ? false
, localPostgresTrust ? false
, recommendedGzipSettings ? false
, tls ? {}
, acme ? {}
}:
{ config, lib, pkgs, ... }:
let
  tlsCfg = {
    enableACME = false;
    forceSSL = false;
    openFirewall = false;
  } // tls;
  acmeCfg = {
    acceptTerms = false;
    email = null;
  } // acme;
in
{
  imports = [
    ./database.nix
    ./backend.nix
    ./frontend.nix
  ];

  config = lib.mkMerge [
    {
      services.nginx.recommendedGzipSettings = lib.mkForce recommendedGzipSettings;

      services.wedding.database = {
        enable = true;
        port   = ports.database;
        dbName = databaseName;
        user   = databaseName;
        passwordFile = databasePasswordFile;
      };

      services.wedding.backend = {
        enable = true;
        port = ports.backend;
        package = packages.backend;
        databaseUrlFile = databaseUrlFile;
        adminPasswordHashFile = adminPasswordHashFile;
        videoDir = videoDir;
        videoMaxBytes = videoMaxBytes;
        cookieSecure = cookieSecure;
      };

      services.wedding.frontend = {
        enable     = true;
        port       = ports.nginx;
        serverName = serverName;
        staticRoot = packages.staticRoot;
        uploadMaxBodySize = uploadMaxBodySize;
      };

      networking.firewall.allowedTCPPorts = lib.mkIf openFirewall [ ports.nginx ];
    }

    (lib.mkIf localHostAlias {
      networking.hosts."127.0.0.1" = [ serverName ];
    })

    (lib.mkIf localPostgresTrust {
      services.postgresql.authentication = lib.mkAfter ''
        host all ${databaseName} 127.0.0.1/32 trust
        host all ${databaseName} ::1/128      trust
      '';
    })

    (lib.mkIf (tlsCfg.enableACME || tlsCfg.forceSSL) {
      services.nginx.virtualHosts.${serverName} = {
        enableACME = tlsCfg.enableACME;
        forceSSL = tlsCfg.forceSSL;
        listen = lib.mkAfter [
          { addr = "0.0.0.0"; port = 443; ssl = true; }
          { addr = "[::]"; port = 443; ssl = true; }
        ];
      };
    })

    (lib.mkIf tlsCfg.openFirewall {
      networking.firewall.allowedTCPPorts = [ 443 ];
    })

    (lib.mkIf tlsCfg.enableACME {
      security.acme = {
        acceptTerms = acmeCfg.acceptTerms;
      } // lib.optionalAttrs (acmeCfg.email != null) {
        defaults.email = acmeCfg.email;
      };
    })
  ];
}
