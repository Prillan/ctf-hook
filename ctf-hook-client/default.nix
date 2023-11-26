{ nixpkgs ? import ./../nixpkgs.nix }:
let pythonPackages = nixpkgs.python3Packages;
in pythonPackages.buildPythonApplication rec {
  pname = "ctf-hook-client";
  version = "0.2.3";

  src = builtins.filterSource (path: type:
    let f = builtins.baseNameOf path;
    in f == "ctf-hook" || f == "setup.py") ./.;

  dontUseSetuptoolsCheck = true;
  propagatedBuildInputs = with pythonPackages; [ inotify-simple requests ];
}
