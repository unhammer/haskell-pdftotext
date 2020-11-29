# This configuration is based on the template in hs-nix-template by
# Utku Demir: <https://github.com/utdemir/hs-nix-template>
{ compiler ? "ghc883" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
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

  shell = myHaskellPackages.shellFor {
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

    # Hoogle fails to build. Uncomment this line if it succeeds

    # withHoogle = true;
    withHoogle = false;

    shellHook = (
      (import sources."pre-commit-hooks.nix").run {
        src = gitignore ./.;
        excludes = [ "^nix/sources\.nix$" ];
        hooks = {
          ormolu.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          nixpkgs-fmt.enable = true;
          nix-linter.enable = true;
          shellcheck.enable = true;
        };
      }
    ).shellHook;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.pdftotext);

in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  pdftotext = myHaskellPackages.pdftotext;
}
