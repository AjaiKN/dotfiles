# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# TODO: maybe https://wiki.nixos.org/wiki/Flakes#Using_nix_flakes_with_NixOS

{
  config,
  lib,
  pkgs,
  ...
}:

let
  # TODO: deduplicate in other file
  user = "ajai";
in

{
  imports = [
    # Include the results of the hardware scan.
    #./hardware-configuration.nix
    ./hardware-configuration.nix
    ./modules/basics.nix
    ./modules/emacs.nix
    ./modules/fonts.nix
    ./modules/users.nix
    ./modules/vm.nix
  ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  # services.xserver.desktopManager.xfce.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;
  # services.xserver.desktopManager.mate.enable = true;
  # services.xserver.windowManager.xmonad.enable = true;
  # services.xserver.windowManager.twm.enable = true;
  # services.xserver.windowManager.icewm.enable = true;
  # services.xserver.windowManager.i3.enable = true;
  # services.xserver.windowManager.herbstluftwm.enable = true;
  # programs.sway.enable = true;
  # programs.hyprland = {
  #   enable = true;
  #   withUWSM = true; # recommended for most users
  #   xwayland.enable = true; # Xwayland can be disabled.
  # };

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # editors
    neovim
    vim
    # tools
    curl
    gcc
    gh
    git
    gnumake
    tree
    wget
    # shells
    zsh
    # fonts
    corefonts
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  programs.zsh.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # https://nixos.org/manual/nixos/stable/#sec-nix-gc
  nix.gc.automatic = true;
  nix.gc.dates = "05:15";

  environment.enableAllTerminfo = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
