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
      rev = "9641db475d8113fd6e1c28612fe51b05f069fab6";
      ref = "master";
    };
  }
