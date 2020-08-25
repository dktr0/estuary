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
      rev = "0ec6c44dc02c83da530accb8a9c418a912658975";
      ref = "main";
    };
  }
