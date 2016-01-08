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

      http = {
        port = mkOption {
          type = with types; uniq int;
          description = "The TCP port to listen on";
        };
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
        PORT = toString cfg.http.port;
        STATE_DIR = cfg.stateDir;
      };

      serviceConfig = {
        User = cfg.user.name;
        PermissionsStartOnly = true;
        Restart = "on-failure";
        RestartSec = 5;
        StartLimitInterval = "1min";
        WorkingDirectory = cfg.stateDir;
      };

      preStart = ''
        mkdir -p -m 0755 ${cfg.stateDir}
        chown -R ${cfg.user.name} ${cfg.stateDir}
      '';

      script = ''
        ${cfg.package}/bin/jude-web
      '';
    };
  };
}
