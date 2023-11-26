{ config, lib, pkgs, ... }:

with builtins;
with lib; {
  config = {
    wsl = {
      enable = true;
      wslConf.automount.root = "/mnt";
      defaultUser = "ariel";
      startMenuLaunchers = true;
      nativeSystemd = true;
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
    };
    programs.dconf.enable = true;
    environment = {
      systemPackages = with pkgs; [
        wsl-open
        wget
        curl
        git
        eza
        man-pages
        man-pages-posix
      ];
      variables = rec {
        BROWSER = "wsl-open";
      };
    };
    services = {
      postgresql = {
        enable = true;
        package = pkgs.postgresql_13;
        enableTCPIP = true;
        authentication = pkgs.lib.mkOverride 13 ''
          local all all trust
          host all all 127.0.0.1/32 trust
          host all all ::1/128 trust
        '';
      };
    };
    systems.stateVersion = "23.11";
  };
}
