{ config, lib, ... }:

# Wedding vhost, split out of frontend.nix (which is kept for the legacy
# factory interface). Serves the GHCJS static site, the admin bundle under
# /admin/, and proxies /api/ to the backend. Adds security headers, HSTS
# when TLS is forced, and rate limiting on the admin login endpoint.

let
  cfg        = config.services.wedding.nginx;
  backendCfg = config.services.wedding.backend;

  proxyHeaders = ''
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
  '';
in {
  options.services.wedding.nginx = {
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

    adminStaticRoot = lib.mkOption {
      type        = lib.types.path;
      description = "Path to the GHCJS-built wedding admin static directory.";
    };

    uploadMaxBodySize = lib.mkOption {
      type        = lib.types.str;
      default     = "200m";
      description = "nginx client_max_body_size for wedding API uploads.";
    };

    enableACME = lib.mkOption {
      type        = lib.types.bool;
      default     = false;
      description = "Enable ACME certificate provisioning for the virtual host.";
    };

    acmeEmail = lib.mkOption {
      type        = lib.types.nullOr lib.types.str;
      default     = null;
      description = "Email address used for ACME registration.";
    };

    forceSSL = lib.mkOption {
      type        = lib.types.bool;
      default     = false;
      description = "Redirect HTTP traffic to HTTPS on this virtual host.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = (!cfg.enableACME) || (cfg.acmeEmail != null);
        message = "services.wedding.nginx.acmeEmail must be set when enableACME = true.";
      }
    ];

    security.acme = lib.mkIf cfg.enableACME {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    services.nginx = {
      enable = true;

      # limit_req zones must be declared in the http{} context.
      appendHttpConfig = ''
        limit_req_zone $binary_remote_addr zone=wedding_login:10m rate=5r/m;
      '';

      virtualHosts.${cfg.serverName} = {
        listen = [
          { addr = "0.0.0.0"; port = cfg.port; }
          { addr = "[::]";   port = cfg.port; }
        ] ++ lib.optionals (cfg.enableACME || cfg.forceSSL) [
          { addr = "0.0.0.0"; port = 443; ssl = true; }
          { addr = "[::]";   port = 443; ssl = true; }
        ];

        enableACME = cfg.enableACME;
        forceSSL   = cfg.forceSSL;

        root = toString cfg.staticRoot;

        extraConfig = ''
          access_log /var/log/nginx/wedding.access.log;
          error_log  /var/log/nginx/wedding.error.log;

          add_header X-Content-Type-Options "nosniff" always;
          add_header X-Frame-Options "SAMEORIGIN" always;
          add_header Referrer-Policy "strict-origin-when-cross-origin" always;
        '' + lib.optionalString cfg.forceSSL ''
          add_header Strict-Transport-Security "max-age=63072000; includeSubDomains" always;
        '';

        locations = {
          "= /admin" = {
            return = "302 /admin/";
          };

          "/" = {
            tryFiles = "$uri $uri/ /index.html";
          };

          # Exact match wins over the /api/ prefix: throttle login attempts.
          "= /api/admin/login" = {
            proxyPass = "http://127.0.0.1:${toString backendCfg.port}";
            extraConfig = ''
              limit_req zone=wedding_login burst=5 nodelay;
              limit_req_status 429;
              ${proxyHeaders}
            '';
          };

          "/api/" = {
            proxyPass = "http://127.0.0.1:${toString backendCfg.port}";
            extraConfig = ''
              client_max_body_size ${cfg.uploadMaxBodySize};
              proxy_request_buffering off;
              proxy_read_timeout 600s;
              ${proxyHeaders}
            '';
          };

          "/admin/" = {
            alias = "${toString cfg.adminStaticRoot}/";
            tryFiles = "$uri $uri/ /admin/index.html";
          };
        };
      };
    };
  };
}
