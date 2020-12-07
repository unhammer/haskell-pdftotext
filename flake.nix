{
  description = "Extracts text from PDF using poppler";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;

  outputs = { nixpkgs, flake-utils, flake-compat, ... }:
    (
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
            myHaskellPackages = pkgs.haskell.packages.ghc883.override {
              overrides = hself: _hsuper: {
                pdftotext =
                  hself.callCabal2nix
                    "pdftotext"
                    (gitignore ./.)
                    {
                      poppler-cpp = pkgs.poppler;
                    };
              };
            };
          in
            rec {
              packages.pdftotext = pkgs.haskell.lib.justStaticExecutables myHaskellPackages.pdftotext;
              defaultPackage = packages.pdftotext;
              apps.pdftotext = flake-utils.lib.mkApp {
                drv = packages.pdftotext;
              };
              defaultApp = apps.pdftotext;
              lib.haskellPackages = myHaskellPackages;
              lib.pdftotext = lib.haskellPackages.pdftotext;
              devShell = myHaskellPackages.shellFor {
                packages = p: [
                  p.pdftotext
                ];
                buildInputs = [
                  pkgs.haskellPackages.cabal-install
                  pkgs.haskellPackages.ghcid
                  pkgs.haskellPackages.ormolu
                  pkgs.haskellPackages.hlint
                  pkgs.niv
                  pkgs.nixpkgs-fmt
                ];
                withHoogle = false;
                # TODO: Add shellHook for installing the pre-commit hook
                # currently defined in setup-hooks.nix
                #
                # See https://github.com/cachix/pre-commit-hooks.nix/pull/67
              };
            }
      )
    )
    // { inherit flake-compat; };
}
