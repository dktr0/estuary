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
      rev = "250e4c73018f7fea791af7713928e9e1dba69303";
      ref = "master";
    };
  }
