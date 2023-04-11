{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    mypython = pkgs.python310;
  in with pkgs; with mypython.pkgs; {
    devShell.x86_64-linux = mkShell { 
      buildInputs = [
          elfkickers
          gfortran
          valgrind
          gdb
          nasm
          mypython
          numpy
          cargo
      ];
    };
  };

}
