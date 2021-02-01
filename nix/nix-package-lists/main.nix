pkgs: with pkgs; [
  # editors
  neovim
  vim
  # apps
  wezterm
  # tools
  curl
  fzf
  gcc
  gh
  git
  git-branchless
  gnumake
  haskellPackages.git-mediate
  nixfmt-rfc-style
  ripgrep
  stow
  tree
  wget
  # shells
  fish
  zsh
  shellcheck
  # fonts
  iosevka-bin
  (iosevka-bin.override { variant = "SGr-IosevkaTerm"; })
]
