{
  config,
  lib,
  pkgs,
  ...
}:

let
  fonts = (import ../../nix-package-lists/fonts.nix pkgs);
in

{
  environment.systemPackages = fonts;
  fonts.packages = fonts;
}
