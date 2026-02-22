{
  inputs = {

    nixpkgs = {
      url = "nixpkgs";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
    };

    haskell-nixpkgs-improvements = {
      url = "github:sergv/haskell-nixpkgs-improvements" ;

      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
      inputs.haskellNix.follows = "haskellNix";
    };

  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, haskell-nixpkgs-improvements }:
    flake-utils.lib.eachSystem ["x86_64-linux" "i686-linux"] (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          ghc = haskell-nixpkgs-improvements.haskell-package-sets."${system}".host.ghc914-pie.ghc;
      in {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.rure
            pkgs.rure.dev
            pkgs.pkg-config
            ghc
          ];
          LD_LIBRARY_PATH = "${pkgs.rure}/lib";
          # shellHook = ''
          #   echo "Updated rure-dev is at ${pkgs.rure.dev}"
          # '';
        };
      });
}
