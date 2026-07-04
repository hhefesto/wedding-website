# Canonical profile module for the wedding RSVP stack.
#
# Consumed as `inputs.wedding-page.nixosModules.default`; the flake applies
# this file to its own `self` so packages and secret paths default correctly:
#
#   services.wedding.profile = {
#     enable = true;
#     mode = "production";                 # or "development"
#     serverName = "xty-y-dan.net";
#     ports = { nginx = 80; backend = 3001; };
#   };
#
# Production mode declares its own agenix secrets (files under
# ${self}/secrets/*.age, mode 0400) — the consumer must import
# agenix.nixosModules.default. Development mode adds a local hosts alias and
# postgres trust auth for the wedding user.
#
# Postgres-sharing invariant: every project on the same host MUST agree on
# `ports.database` (NixOS cannot merge a single-valued port across modules).

self:
{ config, lib, pkgs, options, ... }:

let
  cfg = config.services.wedding.profile;

  production  = cfg.mode == "production";
  development = cfg.mode == "development";

  selfPackages = self.packages.${pkgs.system};

  defaultVideoDir = "/var/lib/wedding/videos";

  dbPasswordFile =
    if cfg.secrets.dbPasswordFile != null
    then cfg.secrets.dbPasswordFile
    else config.age.secrets.wedding-db-password.path;

  backendEnvFile =
    if cfg.secrets.backendEnvFile != null
    then cfg.secrets.backendEnvFile
    else config.age.secrets.wedding-backend-env.path;

  adminHashFile =
    if cfg.secrets.adminPasswordHashFile != null
    then cfg.secrets.adminPasswordHashFile
    else config.age.secrets.wedding-admin-password-hash.path;

  secretDecls =
    lib.optionalAttrs (cfg.secrets.dbPasswordFile == null) {
      wedding-db-password = {
        file  = self + "/secrets/wedding-db-password.age";
        owner = "postgres";
        group = "postgres";
        mode  = "0400";
      };
    }
    // lib.optionalAttrs (cfg.secrets.backendEnvFile == null) {
      wedding-backend-env = {
        file  = self + "/secrets/wedding-backend-env.age";
        owner = "root";
        group = "root";
        mode  = "0400";
      };
    }
    // lib.optionalAttrs (cfg.secrets.adminPasswordHashFile == null) {
      wedding-admin-password-hash = {
        file  = self + "/secrets/wedding-admin-password-hash.age";
        owner = "root";
        group = "root";
        mode  = "0400";
      };
    };

  hardening = {
    NoNewPrivileges = true;
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    ProtectControlGroups = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    LockPersonality = true;
    RestrictSUIDSGID = true;
    RestrictRealtime = true;
    SystemCallArchitectures = "native";
    UMask = "0077";
  };
in {
  imports = [
    ./database.nix
    ./backend.nix
    ./nginx.nix
  ];

  options.services.wedding.profile = {
    enable = lib.mkEnableOption "Wedding RSVP stack profile";

    mode = lib.mkOption {
      type = lib.types.enum [ "development" "production" ];
      description = "Deployment mode used to select secure defaults.";
    };

    serverName = lib.mkOption {
      type = lib.types.str;
      description = "Public DNS name (vhost) served by nginx.";
    };

    ports = {
      nginx = lib.mkOption {
        type = lib.types.port;
        description = "Public HTTP port for nginx.";
      };

      backend = lib.mkOption {
        type = lib.types.port;
        description = "Port the wedding backend listens on.";
      };

      database = lib.mkOption {
        type = lib.types.port;
        default = 5432;
        description = "PostgreSQL port (must match other co-located projects).";
      };
    };

    database = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "wedding";
        description = "Database name.";
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = cfg.database.name;
        description = "PostgreSQL user (defaults to the database name).";
      };
    };

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      default = "hhefesto@rdataa.com";
      description = "Email used for ACME registration in production mode.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Open the nginx port (and 443 in production) in the firewall.";
    };

    videoDir = lib.mkOption {
      type = lib.types.str;
      default = defaultVideoDir;
      description = "Directory where uploaded wedding videos are stored.";
    };

    videoMaxBytes = lib.mkOption {
      type = lib.types.int;
      default = 200 * 1024 * 1024;
      description = "Maximum accepted video upload size in bytes.";
    };

    uploadMaxBodySize = lib.mkOption {
      type = lib.types.str;
      default = "200m";
      description = "nginx client_max_body_size for wedding API uploads.";
    };

    packages = {
      backend = lib.mkOption {
        type = lib.types.package;
        default = selfPackages.wedding-backend;
        description = "The wedding-backend executable package.";
      };

      staticRoot = lib.mkOption {
        type = lib.types.package;
        default = selfPackages.website;
        description = "Static wedding website bundle.";
      };

      adminStaticRoot = lib.mkOption {
        type = lib.types.package;
        default = selfPackages.admin-website;
        description = "Static admin frontend bundle.";
      };
    };

    secrets = {
      dbPasswordFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Override for the PostgreSQL password file (production defaults to the in-repo agenix secret).";
      };

      backendEnvFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Override for the backend EnvironmentFile containing DATABASE_URL.";
      };

      adminPasswordHashFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Override for the bcrypt admin password hash file.";
      };
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services.wedding.database = {
        enable = true;
        port   = cfg.ports.database;
        dbName = cfg.database.name;
        user   = cfg.database.user;
      };

      services.wedding.backend = {
        enable        = true;
        port          = cfg.ports.backend;
        package       = cfg.packages.backend;
        videoDir      = cfg.videoDir;
        videoMaxBytes = cfg.videoMaxBytes;
        cookieSecure  = production;
        publicBaseUrl = "${if production then "https" else "http"}://${cfg.serverName}";
      };

      services.wedding.nginx = {
        enable            = true;
        serverName        = cfg.serverName;
        port              = cfg.ports.nginx;
        staticRoot        = cfg.packages.staticRoot;
        adminStaticRoot   = cfg.packages.adminStaticRoot;
        uploadMaxBodySize = cfg.uploadMaxBodySize;
        enableACME        = production;
        forceSSL          = production;
        acmeEmail         = cfg.acmeEmail;
      };

      # Wedding historically forces gzip off (GHCJS bundles; wins over
      # co-located modules that default it on). Revisit centrally if needed.
      services.nginx.recommendedGzipSettings = lib.mkForce false;

      networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall
        ([ cfg.ports.nginx ] ++ lib.optional production 443);
    }

    (lib.mkIf development {
      networking.hosts."127.0.0.1" = [ cfg.serverName ];

      services.postgresql.authentication = lib.mkAfter ''
        host all ${cfg.database.user} 127.0.0.1/32 trust
        host all ${cfg.database.user} ::1/128      trust
      '';
    })

    # `options ? age` guards the attribute so this module evaluates without
    # agenix (e.g. development mode); production asserts agenix is present.
    (lib.mkIf production (lib.optionalAttrs (options ? age) {
      age.secrets = secretDecls;
    }))

    (lib.mkIf production {
      assertions = [
        {
          assertion = options ? age;
          message = "services.wedding.profile production mode requires the agenix NixOS module to be imported.";
        }
      ];

      services.wedding.database.passwordFile = dbPasswordFile;
      services.wedding.backend.databaseUrlFile = backendEnvFile;

      # The backend runs as DynamicUser and cannot read a 0400 root-owned
      # secret directly; systemd hands it a private copy.
      services.wedding.backend.adminPasswordHashFile =
        "/run/credentials/wedding-backend.service/admin-hash";

      systemd.services.wedding-backend.serviceConfig = hardening
        // {
          LoadCredential = [ "admin-hash:${adminHashFile}" ];
        }
        // lib.optionalAttrs (cfg.videoDir != defaultVideoDir) {
          ReadWritePaths = [ cfg.videoDir ];
        };

      systemd.services.wedding-migrate.serviceConfig = hardening;
    })
  ]);
}
