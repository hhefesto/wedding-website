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
      # Cloudflare (free plan) rejects request bodies over 100 MB.
      default     = 95 * 1024 * 1024;
      description = "Maximum accepted video upload size in bytes.";
    };

    mediaDir = lib.mkOption {
      type        = lib.types.str;
      default     = "/var/lib/wedding/media";
      description = "Directory for guest photo/video uploads (originals and public derivatives).";
    };

    mediaMaxBytes = lib.mkOption {
      type        = lib.types.int;
      default     = 4 * 1024 * 1024 * 1024;
      description = "Maximum size of a single guest photo/video upload, in bytes.";
    };

    mediaMinFreeBytes = lib.mkOption {
      type        = lib.types.int;
      default     = 5 * 1024 * 1024 * 1024;
      description = "Refuse new guest uploads when free disk space would drop below this many bytes.";
    };

    cookieSecure = lib.mkOption {
      type        = lib.types.bool;
      default     = false;
      description = "Whether to mark the admin session cookie as Secure.";
    };

    publicBaseUrl = lib.mkOption {
      type        = lib.types.str;
      default     = "http://wedding.local";
      description = "Public URL used when generating invitee RSVP QR codes.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.wedding.database.enable = lib.mkDefault true;

    systemd.services.wedding-migrate = {
      description = "Wedding RSVP schema migrations";
      wantedBy    = [ "multi-user.target" ];
      before      = [ "wedding-backend.service" ];
      after       = [ "postgresql.service" "postgresql-setup.service" ];
      requires    = [ "postgresql.service" "postgresql-setup.service" ];

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
      after       = [ "postgresql.service" "postgresql-setup.service" "wedding-migrate.service" ];
      requires    = [ "postgresql.service" "postgresql-setup.service" "wedding-migrate.service" ];

      environment = {
        WEDDING_PORT = toString cfg.port;
        WEDDING_VIDEO_DIR = cfg.videoDir;
        WEDDING_VIDEO_MAX_BYTES = toString cfg.videoMaxBytes;
        WEDDING_MEDIA_DIR = cfg.mediaDir;
        WEDDING_MEDIA_MAX_BYTES = toString cfg.mediaMaxBytes;
        WEDDING_MEDIA_MIN_FREE_BYTES = toString cfg.mediaMinFreeBytes;
        WEDDING_COOKIE_SECURE = if cfg.cookieSecure then "true" else "false";
        WEDDING_QRENCODE_BIN = "${pkgs.qrencode}/bin/qrencode";
        WEDDING_PUBLIC_BASE_URL = cfg.publicBaseUrl;
      } // lib.optionalAttrs (cfg.databaseUrlFile == null) {
        DATABASE_URL = cfg.databaseUrl;
      } // lib.optionalAttrs (cfg.adminPasswordHashFile != null) {
        WEDDING_ADMIN_PASSWORD_HASH_FILE = cfg.adminPasswordHashFile;
      };

      # Guest media processing: vipsthumbnail (HEIC via libheif), ffmpeg and
      # ffprobe for video, df for the free-space guard, nice for transcodes.
      path = [ pkgs.vips pkgs.ffmpeg-headless pkgs.coreutils ];

      serviceConfig = {
        ExecStart   = "${cfg.package}/bin/wedding-backend";
        Restart     = "on-failure";
        DynamicUser = true;
        StateDirectory =
          lib.optional (cfg.videoDir == "/var/lib/wedding/videos") "wedding/videos"
          ++ lib.optional (cfg.mediaDir == "/var/lib/wedding/media") "wedding/media";
      } // lib.optionalAttrs (cfg.databaseUrlFile != null) {
        EnvironmentFile = cfg.databaseUrlFile;
      };
    };
  };
}
