{
  config,
  pkgs,
  lib,
  ...
}:
let
  stdenv = pkgs.stdenv;
in
{
  home.username = "ariel";
  home.homeDirectory = if stdenv.hostPlatform.isDarwin then "/Users/ariel" else "/home/ariel";

  # Do not change this value, even when updating Home Manager; check the
  # release notes first if you ever want to bump it.
  home.stateVersion = "23.05";
  home.enableNixpkgsReleaseCheck = false;

  nixpkgs.config.allowUnfree = true;

  home.packages =
    with pkgs;
    [
      nixfmt
      gnupg
      openssh
      graphviz
      nodejs
      bottom
      ripgrep
      fd
      coreutils
      clang
      clang-tools
      pkg-config
      bear
      prettier
      rustup
      omnisharp-roslyn
      maven
      gradle
      cmake
      tetex
      gnumake
      unzip
      antigravity-cli
    ]
    ++ lib.optional (!stdenv.hostPlatform.isDarwin) gdb
    ++ [
      # Update flake inputs, then switch.
      (writeShellApplication {
        name = "update-home";
        text = ''
          nix flake update --flake ~/dotfiles/home-manager
          home-manager switch --flake ~/dotfiles/home-manager
        '';
      })
      # Switch to the current config without updating inputs.
      (writeShellApplication {
        name = "hms";
        text = ''
          home-manager switch --flake ~/dotfiles/home-manager
        '';
      })
      # Update antigravity-cli sources.json.
      (writeShellApplication {
        name = "update-antigravity-cli";
        runtimeInputs = [
          curl
          jq
        ];
        text = ''
          set -euo pipefail
          DOTFILES_DIR="$HOME/dotfiles"
          UPDATE_SCRIPT="$DOTFILES_DIR/home-manager/packages/antigravity-cli/update.sh"

          if [ ! -f "$UPDATE_SCRIPT" ]; then
            echo "Error: Could not find update.sh at $UPDATE_SCRIPT"
            exit 1
          fi

          echo "Running antigravity-cli update script..."
          "$UPDATE_SCRIPT"
        '';
      })
    ];

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    autocd = true;
    historySubstringSearch.enable = true;
    initContent = lib.mkMerge [
      (lib.mkBefore ''
        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
          . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi
        if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
          . "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi
      '')
      ''
        # Launch emacs in the background, passing through any arguments.
        emacs() { command emacs "$@" &! }
      ''
    ];
    profileExtra = lib.mkIf stdenv.hostPlatform.isDarwin ''
      eval "$(/opt/homebrew/bin/brew shellenv)"
    '';
  };

  programs.starship.enable = true;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.pdf-tools
    ];
  };

  programs.java = {
    enable = true;
    package = pkgs.jdk25;
  };

  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "Ariel Raudsepp";
        email = "ariel.raudsepp@gmail.com";
      };
      core = {
        autocrlf = "input";
        editor = "emacs";
      }
      // lib.optionalAttrs (!stdenv.hostPlatform.isDarwin) {
        sshCommand = "/mnt/c/Windows/System32/OpenSSH/ssh.exe";
      };
      color.ui = true;
      pull.rebase = true;
      github = {
        user = "arielraudsepp";
      }
      // lib.optionalAttrs (!stdenv.hostPlatform.isDarwin) {
        "api.github.students.cs.ubc.ca".user = "araud01";
      };
    };
  };

  home.file.".npmrc".text = ''
    prefix = ~/.npm-packages
  '';

  home.sessionPath = [
    "$HOME/.config/emacs/bin"
    "$HOME/.npm-packages/bin"
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
