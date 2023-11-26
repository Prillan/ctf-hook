{
  description = "CTF Hook Flake ❄";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux = {
      ctf-hook-server = import ./ctf-hook-server {
        nixpkgs = import nixpkgs { system = "x86_64-linux"; };
      };
      ctf-hook-client = import ./ctf-hook-client {
        nixpkgs = import nixpkgs { system = "x86_64-linux"; };
      };
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.ctf-hook-client;

    devShells.x86_64-linux.ctf-hook-server = import ./ctf-hook-server/shell.nix {
      nixpkgs = import nixpkgs { system = "x86_64-linux"; };
    };
  };
}
