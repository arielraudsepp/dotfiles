{ config, pkgs, lib, ... }:
let stdenv = pkgs.stdenv;
in {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "ariel";
  home.homeDirectory =
    if stdenv.isDarwin then "/Users/ariel" else "/home/ariel";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.
  home.enableNixpkgsReleaseCheck = false;
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    nixfmt-classic
    gnupg
    openssh
    graphviz
    nodejs
    bottom
    git
    ripgrep
    fd
    coreutils
    clang
    nodePackages.prettier
    rustup
    omnisharp-roslyn
    emacs
    cmake
    tetex
    gnumake
    emacsPackages.pdf-tools
    unzip
    aider-chat
    (pkgs.writeScriptBin "update-home" ''
      cd ~/dotfiles/home-manager
      nix flake update --flake .
      home-manager switch
    '')

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

  home.shellAliases = { emacs = "emacs &"; };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    autocd = true;
    historySubstringSearch.enable = true;
    initContent = lib.mkBefore ''
         if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
      . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi
          if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi
    '';
  };

  programs.starship.enable = true;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".gitconfig".source =
      if stdenv.isDarwin then ./.gitconfig-darwin else ./.gitconfig-wsl;
    ".npmrc" = {
      executable = false;
      text = ''
        prefix = ~/.npm-packages
      '';
    };
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

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/ariel/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

}
