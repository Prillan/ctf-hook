{ nixpkgs, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
  f = import ./generated.nix;

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callPackage f { });

in drv
