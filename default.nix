{ nixpkgs ? import ./nixpkgs.nix }:
let
  server = import ./ctf-hook-server { inherit nixpkgs; };
  client = import ./ctf-hook-client { inherit nixpkgs; };
in {
  server = nixpkgs.haskell.lib.justStaticExecutables server;
  client = client;
}
