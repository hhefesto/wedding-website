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
  };

  config = lib.mkIf cfg.enable {
    services.wedding.database.enable = lib.mkDefault true;

    systemd.services.wedding-backend = {
      description = "Wedding RSVP backend";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "postgresql.service" ];
      requires    = [ "postgresql.service" ];

      environment = {
        WEDDING_PORT = toString cfg.port;
      } // lib.optionalAttrs (cfg.databaseUrlFile == null) {
        DATABASE_URL = cfg.databaseUrl;
      };

      serviceConfig = {
        ExecStart   = "${cfg.package}/bin/wedding-backend";
        Restart     = "on-failure";
        DynamicUser = true;
      } // lib.optionalAttrs (cfg.databaseUrlFile != null) {
        EnvironmentFile = cfg.databaseUrlFile;
      };
    };
  };
}
