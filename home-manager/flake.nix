{
  description = "Home Manager configuration of ariel";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, nixos-wsl, ... }:
    let
      makeHome = system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [ ./home.nix ];
        };

    in {
      homeConfigurations."ariel" = makeHome "aarch64-darwin";
      homeConfigurations."ariel@nixos" = makeHome "x86_64-linux";
      nixosConfigurations."cinnabar" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ nixos-wsl.nixosModules.wsl ./wsl.nix ];
      };
    };
}
