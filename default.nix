# wrapper around generated.nix to add some build specific stuff
{ system ? builtins.currentSystem, nixpkgs ? import <nixpkgs> { inherit system; }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  devMode = pkgs.lib.inNixShell;

  haskellPackages_ = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      aeson = pkgs.haskell.lib.dontCheck self.aeson_0_10_0_0;
      mkDerivation = expr: super.mkDerivation (expr // {
        enableLibraryProfiling = devMode;
      });
    };
  };

  sass = pkgs.bundlerEnv {
    name = "sass";
    gemset = ./gemset.nix;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
  };

  myNodePackages = pkgs.nodePackages.override {
    generated = ./nixfiles/node-packages-generated.nix;
    self = myNodePackages;
  };

  drv = with haskellPackages; pkgs.haskell.lib.overrideCabal (callPackage ./nixfiles/pkg.nix {}) (p: {
    src = ./.;
    buildTools = (p.buildTools or []) ++
      [ sass ] ++
      pkgs.lib.optionals devMode [
        cabal-install cabal2nix hlint stylish-haskell
        pkgs.bundix
      ];
  });

in

  if devMode then drv.env else drv
