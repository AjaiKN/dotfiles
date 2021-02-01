{
  description = "A simple NixOS flake";

  inputs = {
    # NixOS official package source, using a stable branch here (May & November versions are stable)
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    {
      # Please replace my-nixos with your hostname
      nixosConfigurations.my-nixos = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          # Import the previous configuration.nix we used,
          # so the old configuration file still takes effect
          ./configuration.nix
        ];
      };
    };
}
