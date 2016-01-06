{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.jude-web;
in
{
  options = {
    services.jude-web = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Run the webapp.
        '';
      };

      package = mkOption {
        type = types.path;
        description = "Package to use.";
      };

      environment = mkOption {
        type = with types; uniq (enum ["Development" "Staging" "Production"]);
        description = ''
          Environment webapp should run in.
        '';
      };

      http = {
        port = mkOption {
          type = with types; uniq int;
          description = "The TCP port to listen on";
        };

        approot = mkOption {
          type = with types; uniq string;
          description = "/ for the webapp";
        };

        staticroot = mkOption {
          type = with types; uniq string;
          description = "The hostname of the static server";
        };
      };

      aws = mkOption {
        type = with types; uniq attrs;
        description = "the AWS configuration";
      };

      authy_key = mkOption {
        type = types.string;
        description = "the Authy API key";
      };

      authy_userid = mkOption {
        type = types.int;
        description = "the Authy user ID";
      };

      user = {
        name = mkOption {
          type = with types; uniq string;
          description = ''
            Who should run the jude-web process.
          '';
        };

        uid = mkOption {
          type = with types; uniq int;
          description = ''
            web user's UID
          '';
        };
      };

      stateDir = mkOption {
        type = types.string;
        description = "Where to put assets in the service.";
        default = "/var/lib/jude-web";
      };
    };
  };

  config = mkIf cfg.enable {
    users.extraUsers."${cfg.user.name}" = {
      description = "jude-web runner.";
      home = "/var/empty";
      createHome = true;
      useDefaultShell = true;
      uid = cfg.user.uid;
    };

    systemd.services.jude-web = {
      wantedBy = [ "multi-user.target" ];
      description = "Run the jude-web server";
      environment = {
        # app stuff
        HOME = "/homeless-shelter";
        PORT = toString cfg.http.port;
        APPROOT = cfg.http.approot;
        STATICROOT = cfg.http.staticroot;

        STATE_DIR = cfg.stateDir;

        # AWS
        AWS_ACCESS_KEY_ID = cfg.aws.key;
        AWS_ACCESS_KEY_SECRET = cfg.aws.secret;

        # Authy
        AUTHY_ENDPOINT = "api.authy.com";
        AUTHY_KEY = cfg.authy_key;
        AUTHY_USERID = builtins.toString cfg.authy_userid;
      };

      serviceConfig = {
        User = cfg.user.name;
        PermissionsStartOnly = true;
        Restart = "on-failure";
        RestartSec = 5;
        StartLimitInterval = "1min";
      };

      preStart = ''
        mkdir -p -m 0755 ${cfg.stateDir}
        chown -R ${cfg.user.name} ${cfg.stateDir}
      '';

      script = ''
        ${cfg.package}/bin/myapi
      '';
    };
  };
}
