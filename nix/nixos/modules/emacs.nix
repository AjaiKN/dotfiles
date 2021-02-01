{
  config,
  lib,
  pkgs,
  ...
}:

{
  nixpkgs.overlays = [
    (lib.mkIf (config.programs.gnupg.agent.enable) pkgs.pinentry-emacs) # in-emacs gnupg prompts
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = import ../../nix-package-lists/emacs.nix pkgs;

  services.emacs.enable = true;

  systemd.user.services.emacsTerm =
    let
      cfg = config.services.emacs // {
        startWithGraphical = false;
      };
    in
    {
      description = "Emacs daemon service for the terminal only";

      serviceConfig = {
        Type = "notify";
        ExecStart = "${pkgs.runtimeShell} -c 'source ${config.system.build.setEnvironment}; exec ${cfg.package}/bin/emacs --fg-daemon=term'";
        # Emacs exits with exit code 15 (SIGTERM), when stopped by systemd.
        SuccessExitStatus = 15;
        Restart = "always";
      };

      unitConfig = lib.optionalAttrs cfg.startWithGraphical {
        After = "graphical-session.target";
      };
    }
    // lib.optionalAttrs cfg.enable {
      wantedBy = if cfg.startWithGraphical then [ "graphical-session.target" ] else [ "default.target" ];
    };
}
