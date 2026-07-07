# Home-manager module: run the Mugge chat client as an always-on, detachable
# background service. Consumers that take this flake as an input only need:
#
#   imports = [ mugge.homeManagerModules.default ];
#   services.mugge-chat.enable = true;
#
# The client runs inside a `dtach` session supervised by a systemd user
# service, so closing the terminal never drops the connection — desktop
# notifications keep firing while detached. Re-open the chat with `mugge`,
# which re-attaches to the live session. Leave a terminal with Ctrl-\ (or by
# closing it) — the connection stays up. `/quit` no longer stops the service;
# use `systemctl --user stop mugge-chat` for that.
self:
{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.services.mugge-chat;
  inherit (pkgs.stdenv.hostPlatform) system;

  # dtach's control socket. %t expands to $XDG_RUNTIME_DIR in the unit; the
  # attach script resolves the same path from the environment.
  socketUnit = "%t/mugge.sock";

  attach = pkgs.writeShellScriptBin "mugge" ''
    set -euo pipefail
    sock="''${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR is not set}/mugge.sock"
    if [ ! -e "$sock" ]; then
      echo "The mugge-chat background service is not running."
      echo "Start it with: systemctl --user start mugge-chat"
      exit 1
    fi
    echo "Attaching to mugge chat. Detach with Ctrl-\\ (the connection stays up)."
    echo "To stop the background service entirely: systemctl --user stop mugge-chat"
    exec ${pkgs.dtach}/bin/dtach -a "$sock" -e '^\'
  '';
in
{
  options.services.mugge-chat = {
    enable = lib.mkEnableOption "the always-on, detachable Mugge chat client";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${system}.mugge-azure;
      defaultText = lib.literalExpression "mugge.packages.\${system}.mugge-azure";
      description = ''
        The Mugge client package to run in the background. Defaults to the
        Azure client, which bakes in the production server host/port, so no
        further configuration is needed.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      attach
      pkgs.dtach
    ];

    systemd.user.services.mugge-chat = {
      Unit = {
        Description = "Mugge chat background client (detachable via dtach)";
        After = [ "network-online.target" ];
        Wants = [ "network-online.target" ];
      };

      Service = {
        Type = "simple";
        # Tells the client it is the shared background instance, so /quit
        # refuses to exit (which would kill the service for good) and points
        # the user at the dtach detach key instead.
        Environment = [ "MUGGE_SERVICE=1" ];
        # Clear a stale socket left by a crashed run before dtach re-binds it.
        ExecStartPre = "${pkgs.coreutils}/bin/rm -f ${socketUnit}";
        # -N: create the session and stay in the foreground (systemd supervises
        # it); no client is attached until `mugge` runs.
        ExecStart = "${pkgs.dtach}/bin/dtach -N ${socketUnit} -e '^\\' ${cfg.package}/bin/mugge-azure";
        Restart = "on-failure";
        RestartSec = 5;
      };

      Install.WantedBy = [ "default.target" ];
    };
  };
}
