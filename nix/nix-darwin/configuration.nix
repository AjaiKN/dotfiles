{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./modules/fonts.nix
    ./modules/users.nix
  ];

  ids.gids.nixbld = 30000;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    # pkgs.vim
  ];

  nix.gc.automatic = true;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Enable alternative shell support in nix-darwin.
  programs.bash.enable = true;
  programs.fish.enable = true;
  programs.nix-index.enable = true;
  programs.zsh.enable = true;

  # I do this myself in my zshrc
  programs.zsh.enableCompletion = false;
  programs.zsh.enableBashCompletion = false;

  # doesn't exist yet in stable nix-darwin
  # environment.enableAllTerminfo = true;

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
