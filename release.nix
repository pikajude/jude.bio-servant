{ jude-web ? { outPath = ./.; }

, supportedCompilers ? [ "default" "ghc801" ]
, supportedPlatforms ? [ "x86_64-linux" "i686-linux" ]
}:

{ build = let inherit ((import <nixpkgs> {}).lib) genAttrs; in

genAttrs supportedCompilers (compiler:
  genAttrs supportedPlatforms (system:
    with import <nixpkgs> { inherit system; };

    let
      haskellPackages = (if compiler == "default"
        then pkgs.haskellPackages
        else pkgs.haskell.packages.${compiler}).override {
          overrides = self: super:
            let servantHEAD = pkgs.fetchgit {
              url = "https://github.com/haskell-servant/servant.git";
              rev = "f5fe9a060cf2e74c9c94eef59c066e6ac913fbe4";
              sha256 = "1v2fwjiyadia3lqhcf2hyab8rn89pxx6g8wzd8awb5jj189dsxrq";
            }; in
          with pkgs.haskell.lib; {
            servant = addBuildDepends (overrideCabal super.servant (drv: {
              version = "0.5";
              editedCabalFile = null;
              revision = null;
              src = "${servantHEAD}/servant";
            })) [ self.http-api-data self.vault ];
            servant-docs = addBuildDepends (overrideCabal super.servant-docs (drv: {
              version = "0.5";
              src = "${servantHEAD}/servant-docs";
            })) [ self.aeson-pretty self.control-monad-omega ];
            servant-server = addBuildDepends (overrideCabal super.servant-server (drv: {
              version = "0.5";
              src = "${servantHEAD}/servant-server";
            })) [ self.should-not-typecheck ];
          };
        };
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
  )
); }
