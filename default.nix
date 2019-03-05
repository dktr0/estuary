(import ./reflex-platform {}).project ({ pkgs, ghc8_4, ... }: {
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

  shellToolOverrides = ghc: super: {
    inherit (ghc8_4) hpack;
  };

  overrides = self: super: {
    estuary-common = pkgs.haskell.lib.overrideCabal super.estuary-common (drv: {
      preConfigure = ''
      ${ghc8_4.hpack}/bin/hpack;
      '';
    });
    # https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid#Writingcompatiblecode
    musicw = pkgs.haskell.lib.dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "SpencerPark"; #owner = "dktr0";
      repo = "MusicW";
      rev = "monoid-compat"; #rev = "94830ba9f5929abbd09094bc5d21d44c6a88a889";
      sha256 = "035vzhf2fiiznbglyahq4jb5as79npwr7fnxpjf2gvki4d35lcr3"; #sha256 = "04k73rx2w04jn5lx8q7b2kjl4yqypvacv4v97bvrggg03kjvgbks";
    }) {});

    punctual = self.callCabal2nix "punctual" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "Punctual";
      rev = "ab0ba53310d6f764bf19490f835623c915a13eb8";
      sha256 = "0jq5s0kb9h1f2m9czl432dcgv914axx2r0cbgsp0079jbawypnzy";
    }) {};

    # microspec >=0.2.0.1 needs jailbreak
    tidal = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "tidal" (pkgs.fetchFromGitHub {
      owner = "TidalCycles";
      repo = "Tidal";
      rev = "65724857fad9983b35dd24fe37fb36a0cb7dbbb8";
      sha256 = "0lahd9rd3pv0xfjl2rxgj8x5ds3b5nl6n5qa68y6niyfrmhh0vg2";
    }) {});

    reflex-dom-contrib = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "bc1b3c0ea689621a0d04b162c9c5ce83497b3ff5";
      sha256 = "0fflq23sr5gxnf2piy3qxdq2aj7bhqacy3skxnw83m01xxq7clnl";
    };

    # Skip tests for the following (they fail).
    Glob = pkgs.haskell.lib.dontCheck (super.Glob);
    mockery = pkgs.haskell.lib.dontCheck (super.mockery);
    silently = pkgs.haskell.lib.dontCheck (super.silently);
    unliftio = pkgs.haskell.lib.dontCheck (super.unliftio);
    conduit = pkgs.haskell.lib.dontCheck (super.conduit);
    yaml = pkgs.haskell.lib.dontCheck (super.yaml);
    hpack = pkgs.haskell.lib.dontCheck (super.hpack);
  };
})
