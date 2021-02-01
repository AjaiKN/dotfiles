{
  config,
  lib,
  pkgs,
  ...
}:

let
  # TODO: deduplicate in other file
  user = "ajainelson";
in

{
  users.users.${user} = {
    description = "Ajai Nelson";
    packages = with pkgs; [
      nixfmt-rfc-style
      parinfer-rust-emacs
    ];
    shell = pkgs.zsh;
  };

  # allow touch ID for sudo
  security.pam.services.sudo_local.touchIdAuth = true;
}
