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
      rev = "d7253e3c0dfcb44915b42b66d8b9a89896eea9c1";
      ref = "main";
    };
  }
