{ config, lib, pkgs, ... }:

# INVARIANT: every project that shares this PostgreSQL instance must agree on
# `services.postgresql.settings.port`. NixOS merges `enable=true` and the
# ensure* lists across modules; the port is a single value with no merge.

let
  cfg = config.services.wedding.database;
in {
  options.services.wedding.database = {
    enable = lib.mkEnableOption "Wedding RSVP PostgreSQL database";

    port = lib.mkOption {
      type        = lib.types.port;
      default     = 5432;
      description = "PostgreSQL listening port (must match other co-located projects).";
    };

    dbName = lib.mkOption {
      type        = lib.types.str;
      default     = "wedding";
      description = "Database name.";
    };

    user = lib.mkOption {
      type        = lib.types.str;
      default     = "wedding";
      description = "PostgreSQL user.";
    };

    passwordFile = lib.mkOption {
      type        = lib.types.nullOr lib.types.path;
      default     = null;
      description = "Path to a file containing the PostgreSQL user password (md5 auth).";
    };
  };

  config = lib.mkIf cfg.enable {
    services.postgresql = {
      enable          = true;
      settings.port   = cfg.port;
      ensureDatabases = [ cfg.dbName ];
      ensureUsers = [
        {
          name              = cfg.user;
          ensureDBOwnership = true;
        }
      ];
    };

    services.postgresql.authentication = lib.mkIf (cfg.passwordFile != null) ''
      local all ${cfg.user} md5
      host  all ${cfg.user} 127.0.0.1/32 md5
      host  all ${cfg.user} ::1/128 md5
    '';

    systemd.services.postgresql.postStart =
      lib.mkIf (cfg.passwordFile != null)
        (lib.mkAfter ''
          for i in $(${pkgs.coreutils}/bin/seq 1 30); do
            if ${pkgs.postgresql}/bin/psql -v ON_ERROR_STOP=1 -d postgres \
                 -tAc "SELECT 1 FROM pg_roles WHERE rolname='${cfg.user}'" \
                 | ${pkgs.gnugrep}/bin/grep -q 1; then
              break
            fi
            ${pkgs.coreutils}/bin/sleep 1
          done
          pw="$(${pkgs.coreutils}/bin/cat ${cfg.passwordFile})"
          ${pkgs.postgresql}/bin/psql -v ON_ERROR_STOP=1 -d postgres \
            -c "ALTER USER ${cfg.user} WITH PASSWORD '$pw';"
        '');
  };
}
