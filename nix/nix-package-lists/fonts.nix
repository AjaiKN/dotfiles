pkgs: with pkgs; [
  iosevka-bin
  (iosevka-bin.override { variant = "SGr-IosevkaTerm"; })
  pkgs.nerd-fonts.symbols-only
  pkgs.nerd-fonts.iosevka
  pkgs.nerd-fonts.iosevka-term
]
