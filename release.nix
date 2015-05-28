{ fnetSrc ? ./.
, officialRelease ? false
}:

let
  pkgs = import <nixpkgs> {};
  siteBuilderDeps = pkgs.haskellPackages.ghcWithPackages (p: with p; [ base filepath hakyll time ]);
in rec {
  build =
    with import <nixpkgs> {};
    releaseTools.nixBuild
    {
      name = "fnet";
      src = fnetSrc;

      phases = [ "unpackPhase" "buildPhase" "installPhase" ];

      buildInputs = [ glibcLocales git siteBuilderDeps ];

      buildPhase = ''
        LANG=en_US.UTF-8 runhaskell site.hs rebuild
      '';

      installPhase = ''
        # Copy generated files to the output directory.
        cp -r _site $out
      '';
    };
}
