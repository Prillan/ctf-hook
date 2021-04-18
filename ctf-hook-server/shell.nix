{ nixpkgs ? import ./../nixpkgs.nix, compiler ? "default" }:
let
  haskellPackages = if compiler == "default" then
    nixpkgs.haskellPackages
  else
    nixpkgs.haskell.packages.${compiler};
in nixpkgs.mkShell {
  buildInputs = [
    nixpkgs.cabal2nix
    haskellPackages.cabal-install
    (nixpkgs.haskell.lib.justStaticExecutables haskellPackages.stylish-haskell)
  ] ++ (import ./. {
    inherit compiler;
    inherit nixpkgs;
  }).env.nativeBuildInputs;
}
