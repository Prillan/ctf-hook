{ nixpkgs, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  myHaskellPackages = haskellPackages.override {
    overrides = hself: hsuper: {
      "ctf-hook-server" = hself.callCabal2nix "ctf-hook" ./. { };
    };
  };
in myHaskellPackages.ctf-hook-server
