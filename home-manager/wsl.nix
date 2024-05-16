{ config, lib, pkgs, ... }:

with builtins;
with lib; {
  config = {
    wsl = {
      enable = true;
      wslConf.automount.root = "/mnt";
      defaultUser = "ariel";
      startMenuLaunchers = true;
      nativeSystemd = false;
    };
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      settings = {
        trusted-users = [ "root" "ariel" ];
        auto-optimise-store = true;
      };
    };
    services = {
      postgresql = {
        enable = true;
        enableTCPIP = true;
        package = pkgs.postgresql_16;
        authentication = pkgs.lib.mkOverride 16 ''
          #type database  DBuser  orgin-address   auth-method
          local all       all                     trust
          host  all       all     127.0.0.1/32    trust
          host  all       all     ::1/128         trust
        '';
      };
      # Use "" to start a standard redis instance
      redis.servers."".enable = true;
      redis.servers."".openFirewall = true;
    };

    users.users.ariel = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = [ "wheel" "docker" ];
    };
    time.timeZone = "America/Vancouver";
    fonts.fontconfig = {
      enable = true;
      localConf = ''
        <dir>/mnt/c/Windows/Fonts</dir>
      '';
    };
    i18n.defaultLocale= "en_CA.UTF-8";
    programs.zsh = {
      enable = true;
      shellAliases = {
      #   ssh = "ssh.exe";
      #   ssh-add = "ssh-add.exe";
        ls = "eza -la";
      };
      loginShellInit = ''
        export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
        '';
    };
    programs.dconf.enable = true;
    virtualisation.docker.enable = true;
    environment = {
      systemPackages = with pkgs; [
        emacs29
	      coreutils
        wsl-open
        wget
        curl
        git
        eza
        man-pages
        man-pages-posix
        hunspell
        hunspellDicts.en_CA
        (pkgs.writeScriptBin "update-system" ''
                nix flake update ~/dotfiles/home-manager
                sudo nixos-rebuild switch --flake ~/dotfiles/home-manager#cinnabar
                '')
        (pkgs.writeScriptBin "update-home" ''
                nix flake update ~/dotfiles/home-manager
                home-manager switch --flake ~/dotfiles/home-manager
                '')

      ];
      variables = rec {
        BROWSER = "wsl-open";
      };
    };
    system.stateVersion = "23.11";
  };
}
