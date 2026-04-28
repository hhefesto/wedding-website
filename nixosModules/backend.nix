{ config, lib, pkgs, ... }:

let
  cfg   = config.services.wedding.backend;
  dbCfg = config.services.wedding.database;
in {
  options.services.wedding.backend = {
    enable = lib.mkEnableOption "Wedding RSVP Servant backend";

    port = lib.mkOption {
      type        = lib.types.port;
      default     = 3001;
      description = "Port the HTTP server listens on.";
    };

    package = lib.mkOption {
      type        = lib.types.package;
      description = "The wedding-backend executable package.";
    };

    databaseUrl = lib.mkOption {
      type    = lib.types.str;
      default = "postgres://${dbCfg.user}@localhost:${toString dbCfg.port}/${dbCfg.dbName}";
      description = "PostgreSQL connection URL.";
    };

    databaseUrlFile = lib.mkOption {
      type        = lib.types.nullOr lib.types.path;
      default     = null;
      description = "Path to an EnvironmentFile containing DATABASE_URL.";
    };

    adminPasswordHashFile = lib.mkOption {
      type        = lib.types.nullOr lib.types.path;
      default     = null;
      description = "Path to a file containing the bcrypt admin password hash.";
    };

    videoDir = lib.mkOption {
      type        = lib.types.str;
      default     = "/var/lib/wedding/videos";
      description = "Directory where uploaded wedding videos are stored.";
    };

    videoMaxBytes = lib.mkOption {
      type        = lib.types.int;
      default     = 200 * 1024 * 1024;
      description = "Maximum accepted video upload size in bytes.";
    };

    cookieSecure = lib.mkOption {
      type        = lib.types.bool;
      default     = false;
      description = "Whether to mark the admin session cookie as Secure.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.wedding.database.enable = lib.mkDefault true;

    systemd.services.wedding-migrate = {
      description = "Wedding RSVP schema migrations";
      wantedBy    = [ "multi-user.target" ];
      before      = [ "wedding-backend.service" ];
      after       = [ "postgresql.service" ];
      requires    = [ "postgresql.service" ];

      environment = {
        MIGRATIONS_DIR = "${cfg.package}/share/wedding-migrations";
      } // lib.optionalAttrs (cfg.databaseUrlFile == null) {
        DATABASE_URL = cfg.databaseUrl;
      };

      serviceConfig = {
        Type            = "oneshot";
        RemainAfterExit = true;
        ExecStart       = "${cfg.package}/bin/wedding-migrate";
        DynamicUser     = true;
      } // lib.optionalAttrs (cfg.databaseUrlFile != null) {
        EnvironmentFile = cfg.databaseUrlFile;
      };
    };

    systemd.services.wedding-backend = {
      description = "Wedding RSVP backend";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "postgresql.service" "wedding-migrate.service" ];
      requires    = [ "postgresql.service" "wedding-migrate.service" ];

      environment = {
        WEDDING_PORT = toString cfg.port;
        WEDDING_VIDEO_DIR = cfg.videoDir;
        WEDDING_VIDEO_MAX_BYTES = toString cfg.videoMaxBytes;
        WEDDING_COOKIE_SECURE = if cfg.cookieSecure then "true" else "false";
      } // lib.optionalAttrs (cfg.databaseUrlFile == null) {
        DATABASE_URL = cfg.databaseUrl;
      } // lib.optionalAttrs (cfg.adminPasswordHashFile != null) {
        WEDDING_ADMIN_PASSWORD_HASH_FILE = cfg.adminPasswordHashFile;
      };

      serviceConfig = {
        ExecStart   = "${cfg.package}/bin/wedding-backend";
        Restart     = "on-failure";
        DynamicUser = true;
        StateDirectory = lib.mkIf (cfg.videoDir == "/var/lib/wedding/videos") "wedding/videos";
      } // lib.optionalAttrs (cfg.databaseUrlFile != null) {
        EnvironmentFile = cfg.databaseUrlFile;
      };
    };
  };
}
