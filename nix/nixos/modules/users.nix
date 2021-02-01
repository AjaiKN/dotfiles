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
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${user} = {
    isNormalUser = true;
    description = "${user}";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    packages = import ../../nix-package-lists/main.nix pkgs;
    shell = pkgs.zsh;
  };
  # users.defaultUserShell = pkgs.zsh;

  # allow sudo without password
  security.sudo.extraRules = [
    {
      users = [ user ];
      commands = [
        {
          command = "ALL";
          options = [
            "SETENV"
            "NOPASSWD"
          ];
        }
      ];
    }
  ];
}
