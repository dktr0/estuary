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
      rev = "c5124ab0d2a5a5c9fb7d0f1211673598bb69f684";
      ref = "main";
    };
  }
