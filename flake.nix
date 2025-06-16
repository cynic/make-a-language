{
  description = "Dev environment with Node.js 24 (nixos-unstable)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; # Unstable channel
    flake-utils.url = "github:numtide/flake-utils";       # Simplifies outputs
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            nodejs_24   # Node.js 24 from nixos-unstable
          ];

          shellHook = ''
            echo "Node.js $(node --version) ready!"
          '';
        };
      }
    );
}