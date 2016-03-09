{ pkgs ? import <nixpkgs> {}, compiler ? "default", jude-web ? { outPath = ./.; } }:

let
  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};
  build = haskellPackages.callPackage ./default.nix {};
  bowerPkgs = pkgs.callPackage ./bower.nix {};

  tarball = with pkgs; releaseTools.sourceTarball rec {
    name = build.pname;
    version = build.version;
    src = jude-web;
    buildInputs = [ git jq ];

    postUnpack = ''
      # Clean up when building from a working tree.
      if [[ -d $sourceRoot/.git ]]; then
        git -C $sourceRoot clean -fdx
      fi
    '';

    preDist = ''
      mkdir -p bower_components
      for pkg in ${bowerPkgs}/packages/*/*; do
        name="$(jq -r .name $pkg/.bower.json)"
        cp -RL "$pkg" bower_components/"$name"
      done
    '';

    distPhase = ''
      runHook preDist
      tar cfj tarball.tar.bz2 * --transform 's,^,${name}/,'
      mkdir -p $out/tarballs
      cp *.tar.* $out/tarballs
    '';
  };

in pkgs.haskell.lib.overrideCabal build (drv: {
  configureFlags = [ "-fopt" ];
  doHaddock = false;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  isLibrary = false;
  src = "${tarball}/tarballs/*.tar.bz2";
})
