{
  config,
  lib,
  pkgs,
  ...
}:

let
  fonts = (import ../../nix-package-lists/fonts.nix pkgs) ++ [ pkgs.corefonts ];
in

{
  environment.systemPackages = fonts;
  fonts.packages = fonts;
  fonts.enableDefaultPackages = true;
  fonts.fontDir.enable = true; # not really sure what this is or if it's necessary
}
