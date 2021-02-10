let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in
  stdenv.mkDerivation {
    name = "pegjs";
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir $out
      cp -Rf $src $out/peg-0.10.0.min.js
    '';
    src = builtins.fetchurl {
      url = "https://github.com/pegjs/pegjs/releases/download/v0.10.0/peg-0.10.0.min.js";
    };
  }
