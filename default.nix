(import ./reflex-platform {}).project ({ pkgs, ... }: {
  name = "Estuary";

  packages = {
    estuary = ./client;
    estuary-common = ./common;
    estuary-server = ./server;
  };

  shells = {
    ghc = ["estuary" "estuary-common" "estuary-server"];
    ghcjs = ["estuary" "estuary-common"];
  };

  overrides = self: super: {
    musicw = self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "MusicW";
      rev = "94830ba9f5929abbd09094bc5d21d44c6a88a889";
      sha256 = "04k73rx2w04jn5lx8q7b2kjl4yqypvacv4v97bvrggg03kjvgbks";
    }) {};

    punctual = self.callCabal2nix "punctual" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "Punctual";
      rev = "ab0ba53310d6f764bf19490f835623c915a13eb8";
      sha256 = "0jq5s0kb9h1f2m9czl432dcgv914axx2r0cbgsp0079jbawypnzy";
    }) {};

    tidal = self.callCabal2nix "tidal" (pkgs.fetchFromGitHub {
      owner = "TidalCycles";
      repo = "Tidal";
      rev = "65724857fad9983b35dd24fe37fb36a0cb7dbbb8";
      sha256 = "0lahd9rd3pv0xfjl2rxgj8x5ds3b5nl6n5qa68y6niyfrmhh0vg2";
    }) {};
  };
})
