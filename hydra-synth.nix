let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in
  stdenv.mkDerivation {
    name = "hydra-synth";
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir $out
      cp -Rf $src/* $out
    '';
    src = builtins.fetchGit {
      url = "https://github.com/ojack/hydra-synth.git";
      rev = "bedeec44959ae7dd94d625655a848df8474967f8";
      ref = "master";
    };
  }
