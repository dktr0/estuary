let pkgs = (import <nixpkgs> {}).pkgs;
in { 
callCabal2nix ? pkgs.haskellPackages.callCabal2nix,
overrideDerivation ? pkgs.lib.overrideDerivation,
... 
}:
let
  base-webdirt-src = import ../github-dep.nix {
    spec = ./github.json;
    extra = {
      name = "webdirt-source";
    };
  };
in
callCabal2nix "webdirt" 
  (pkgs.stdenv.mkDerivation {
    name = "webdirt-src";
    webdirtSrc = base-webdirt-src;
    packageYaml = ./package.yaml;
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup

      mkdir $out
      cp -rf $webdirtSrc/* $out
      cp $packageYaml $out/package.yaml
      ls $out
    '';
  })
  {}
