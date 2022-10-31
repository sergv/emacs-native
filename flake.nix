{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

          hpkgs = pkgs.haskell.packages.ghc943;

          nativeDeps = [
            pkgs.zlib
          ];

      in {
        devShell = pkgs.mkShell {

          buildInputs = nativeDeps;

          nativeBuildInputs = [
            # (hpkgs.ghc.override (oldAttrs: {
            #   enableRelocatedStaticLibs = true;
            #   enableNativeBignum = true;
            #   enableDocs = false;
            # }))
          ] ++ map (x: if builtins.hasAttr "dev" x then x.dev else x) nativeDeps;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeDeps;

          # # Add executable packages to the nix-shell environment.
          # packages = [
          #   # hpkgs.ghc
          #   # hpkgs.cabal-install
          #   pkgs.zlib
          # ];

          # Add build dependencies of the listed derivations to the nix-shell environment.
          # inputsFrom = [ pkgs.hello pkgs.gnutar ];

          # ... - everything mkDerivation has
        };
      });
}
