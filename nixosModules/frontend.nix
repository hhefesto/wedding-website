{ config, lib, ... }:

let
  cfg        = config.services.wedding.frontend;
  backendCfg = config.services.wedding.backend;
in {
  options.services.wedding.frontend = {
    enable = lib.mkEnableOption "Wedding RSVP nginx vhost";

    serverName = lib.mkOption {
      type        = lib.types.str;
      default     = "_";
      description = "Nginx server_name to serve the wedding site under.";
    };

    port = lib.mkOption {
      type        = lib.types.port;
      default     = 80;
      description = "Port nginx listens on for HTTP traffic.";
    };

    staticRoot = lib.mkOption {
      type        = lib.types.path;
      description = "Path to the GHCJS-built wedding website static directory.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;

      virtualHosts.${cfg.serverName} = {
        listen = [
          { addr = "0.0.0.0"; port = cfg.port; }
          { addr = "[::]";   port = cfg.port; }
        ];

        root = toString cfg.staticRoot;

        extraConfig = ''
          access_log /var/log/nginx/wedding.access.log;
          error_log  /var/log/nginx/wedding.error.log;
        '';

        locations = {
          "/" = {
            tryFiles = "$uri $uri/ /index.html";
          };

          "/api/" = {
            proxyPass = "http://127.0.0.1:${toString backendCfg.port}";
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
            '';
          };
        };
      };
    };
  };
}
