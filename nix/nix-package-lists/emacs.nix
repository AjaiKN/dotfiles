pkgs: with pkgs; [
  # NOTE: native comp is currently disabled on Mac in nixpkgs: https://github.com/NixOS/nixpkgs/issues/395169
  (
    (emacsPackagesFor (
      emacs30.override {
        # withNativeCompilation = true;
        # withImageMagick = true;
        # https://github.com/nix-community/emacs-overlay/issues/466
        # withXwidgets = true;
      }
    )).emacsWithPackages
      (epkgs: [
        epkgs.editorconfig
        epkgs.hotfuzz
        epkgs.pdf-tools
        epkgs.treesit-grammars.with-all-grammars
        epkgs.vterm
      ])
  )

  binutils # not sure whether this is needed for native-comp for 'as'

  # Doom dependencies
  # https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
  ripgrep
  gnutls # for TLS connectivity

  # Doom optional dependencies
  fd # faster projectile indexing
  # imagemagick # for image-dired
  zstd # for undo-fu-session/undo-tree compression

  # Doom module dependencies
  editorconfig-core-c # TODO: maybe use https://github.com/editorconfig/editorconfig-core-go instead
  sqlite # :tools lookup & :lang org +roam
  clang-tools # :lang cc
  age # :lang nix
  nixfmt-rfc-style # :lang nix
  parinfer-rust-emacs # :editor parinfer

  # For building vterm
  # gnumake
  # cmake
  # libtool
]
