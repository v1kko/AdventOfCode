{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in with pkgs; {
    devShell.x86_64-linux = mkShell { 
      buildInputs = [
        pkgsCross.aarch64-multiplatform.buildPackages.gdb
        pkgsCross.aarch64-multiplatform.buildPackages.gcc
        pkgsCross.aarch64-multiplatform.buildPackages.glibc_multi
        qemu
      ];
    };
  };

}
