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
}:
{ config, lib, pkgs, ... }:
{
  imports = [
    ./database.nix
    ./backend.nix
    ./frontend.nix
  ];

  services.wedding.database = {
    enable = true;
    port   = ports.database;
    dbName = databaseName;
    user   = databaseName;
  };

  services.wedding.backend = {
    enable  = true;
    port    = ports.backend;
    package = packages.backend;
  };

  services.wedding.frontend = {
    enable     = true;
    port       = ports.nginx;
    serverName = serverName;
    staticRoot = packages.staticRoot;
  };

  networking.firewall.allowedTCPPorts = lib.mkIf openFirewall [ ports.nginx ];
}
