(import ./reflex-platform {}).project ({ pkgs, ghc8_4, hackGet, ... }: {
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
    inherit (ghc8_4) hpack; # always use ghc (not ghcjs) compiled hpack
  };

  overrides = 
    let 
      skipBrokenGhcjsTests = self: super: 
        # generate an attribute set of 
        #   {${name} = pkgs.haskell.lib.dontCheck (super.${name})}
        # if using ghcjs.
        pkgs.lib.genAttrs [
            "Glob" "mockery" "silently" "unliftio" "conduit" 
            "yaml" "hpack"
          ] (name: (if !(self.ghc.isGhcjs or false) then pkgs.lib.id else pkgs.haskell.lib.dontCheck) super.${name});

      manualOverrides = self: super: {
        estuary = pkgs.haskell.lib.overrideCabal super.estuary (drv: {
          preConfigure = ''
          ${ghc8_4.hpack}/bin/hpack;
          '';
        });

        estuary-common = pkgs.haskell.lib.overrideCabal super.estuary-common (drv: {
          preConfigure = ''
          ${ghc8_4.hpack}/bin/hpack;
          '';
        });

        # https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid#Writingcompatiblecode
        musicw = if !(self.ghc.isGhcjs or false) then null
          else pkgs.haskell.lib.dontHaddock 
            (self.callCabal2nix "musicw" (import ./deps/musicw) {});

        punctual = if !(self.ghc.isGhcjs or false) then null
          else self.callCabal2nix "punctual" (import ./deps/punctual) {};

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null
          else pkgs.haskell.lib.doJailbreak 
            (self.callCabal2nix "tidal" (import ./deps/tidal) {});

        # It is a nix package, but use cabal2nix anyways. The nix one 
        # has a bad base constraint.
        reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" (import ./deps/reflex-dom-contrib) {};
      };
    in
      pkgs.lib.composeExtensions skipBrokenGhcjsTests manualOverrides;
})
