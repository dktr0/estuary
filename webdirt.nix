let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in
  stdenv.mkDerivation {
    name = "webdirt-source";
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir $out
      cp -Rf $src/* $out
    '';
    src = builtins.fetchGit {
      url = "https://github.com/dktr0/WebDirt.git";
      rev = "a4781571b92cc9c2318740f3da3081e11c0d1f54";
      ref = "master";
    };
  }
