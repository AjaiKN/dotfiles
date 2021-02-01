# NOTE: I'm not currently using home-manager.

{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "ajai";
  home.homeDirectory = "/home/ajai";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    pkgs.vim
    pkgs.neovim
    pkgs.git
    pkgs.gh
    pkgs.zsh
    pkgs.curl
    pkgs.wget
    # pkgs.make
    pkgs.spice-vdagent
    #pkgs.spice-webdavd
    #pkgs.qemu-guest-agent
    pkgs.ripgrep
    pkgs.emacs
    pkgs.iosevka-bin
    pkgs.gcc

    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file =
    let
      dotfile = (
        file: {
          source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/prog/dotfiles/dot-home/${file}";
        }
      );
      conf = (
        file: {
          source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/prog/dotfiles/config/${file}";
        }
      );
    in
    {
      ".bash_profile" = dotfile ".bash_profile";
      ".bashrc" = dotfile ".bashrc";
      ".bash_zsh_shared_profile.sh" = dotfile ".bash_zsh_shared_profile.sh";
      ".bash_zsh_shared_rc.sh" = dotfile ".bash_zsh_shared_rc.sh";
      ".editrc" = dotfile ".editrc";
      ".fasdrc" = dotfile ".fasdrc";
      ".inputrc" = dotfile ".inputrc";
      ".p10k.mise.zsh" = dotfile ".p10k.mise.zsh";
      ".p10k.zsh" = dotfile ".p10k.zsh";
      ".profile" = dotfile ".profile";
      ".sbclrc" = dotfile ".sbclrc";
      ".sleep" = dotfile ".sleep";
      ".talon" = dotfile ".talon";
      ".tigrc" = dotfile ".tigrc";
      ".tmux.conf" = dotfile ".tmux.conf";
      ".vimrc" = dotfile ".vimrc";
      ".wakeup" = dotfile ".wakeup";
      ".zlogin" = dotfile ".zlogin";
      ".zlogout" = dotfile ".zlogout";
      ".zprofile" = dotfile ".zprofile";
      ".zshenv" = dotfile ".zshenv";
      ".zshrc" = dotfile ".zshrc";

      ".config/bat" = conf "bat";
      ".config/doom" = conf "doom";
      ".config/emacs" = conf "emacs";
      ".config/emacs.basic" = conf "emacs.basic";
      ".config/emacs.minimal" = conf "emacs.minimal";
      ".config/emacs.minimal.config" = conf "emacs.minimal.config";
      ".config/enchant/enchant.ordering" = conf "enchant/enchant.ordering";
      ".config/enchant/README_AKN" = conf "enchant/README_AKN";
      ".config/fish" = conf "fish";
      ".config/gh/config.yml" = conf "gh/config.yml";
      ".config/ghostty" = conf "ghostty";
      ".config/git" = conf "git";
      ".config/hypr" = conf "hypr";
      ".config/i3" = conf "i3";
      ".config/karabiner" = conf "karabiner";
      ".config/keymapper.conf" = conf "keymapper.conf";
      ".config/lazygit" = conf "lazygit";
      ".config/lazyvim" = conf "lazyvim";
      ".config/mise" = conf "mise";
      ".config/nix" = conf "nix";
      ".config/nvim" = conf "nvim";
      ".config/nvim-kickstart" = conf "nvim-kickstart";
      ".config/rstudio" = conf "rstudio";
      ".config/stumpwm" = conf "stumpwm";
      ".config/sway" = conf "sway";
      ".config/talon" = conf "talon";
      ".config/texinfo" = conf "texinfo";
      ".config/thefuck" = conf "thefuck";
      ".config/topgrade.d" = conf "topgrade.d";
      ".config/topgrade.toml" = conf "topgrade.toml";
      ".config/wezterm" = conf "wezterm";
      ".config/zed" = conf "zed";
      ".config/zellij" = conf "zellij";
      ".config/zsh" = conf "zsh";

      # ".config/iterm2"                   = conf "iterm2";

      ".config/katerc" = conf "katerc";
      ".config/kded5rc" = conf "kded5rc";
      ".config/kdeglobals" = conf "kdeglobals";
      ".config/kglobalshortcutsrc" = conf "kglobalshortcutsrc";
      ".config/konsolerc" = conf "konsolerc";

      # # Building this configuration will create a copy of 'dotfiles/screenrc' in
      # # the Nix store. Activating the configuration will then make '~/.screenrc' a
      # # symlink to the Nix store copy.
      # ".screenrc".source = dotfiles/screenrc;

      # # You can also set the file content immediately.
      # ".gradle/gradle.properties".text = ''
      #   org.gradle.console=verbose
      #   org.gradle.daemon.idletimeout=3600000
      # '';
    };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/ajai/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
  };
}
