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

  outputs =
    {
      nixpkgs,
      home-manager,
      nixos-wsl,
      ...
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
        "aarch64-linux"
      ];
      forEachSupportedSystem =
        f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      overlay = final: prev: {
        antigravity-cli = final.callPackage ./packages/antigravity-cli { };
      };

      makeHome =
        system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
            };
            overlays = [ overlay ];
          };
          modules = [ ./home.nix ];
        };

    in
    {
      overlays.default = overlay;

      packages = forEachSupportedSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
            };
            overlays = [ overlay ];
          };
        in
        {
          inherit (pkgs) antigravity-cli;
          default = pkgs.antigravity-cli;
        }
      );

      homeConfigurations."ariel" = makeHome "aarch64-darwin";
      homeConfigurations."ariel@nixos" = makeHome "x86_64-linux";
      nixosConfigurations."cinnabar" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixos-wsl.nixosModules.wsl
          {
            nixpkgs.overlays = [ overlay ];
            nixpkgs.config.allowUnfree = true;
          }
          ./wsl.nix
        ];
      };
    };
}
