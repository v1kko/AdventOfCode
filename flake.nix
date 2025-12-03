{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    mypython = pkgs.python313;
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
          rustc
      ];
      shellHook = ''
        # Allow the use of wheels.
        SOURCE_DATE_EPOCH=$(date +%s)

        # Setup the virtual environment if it doesn't already exist.
        VENV=.venv
        if test ! -d $VENV; then
          python -m venv $VENV
        fi

        export PYTHONPATH=`pwd`/$VENV/${mypython.sitePackages}/:$PYTHONPATH
        source ./$VENV/bin/activate
        '';
    };
  };

}
