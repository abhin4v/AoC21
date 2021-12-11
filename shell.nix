{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.aoc21.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.aoc21.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
