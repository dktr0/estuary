{ reflexPlatformVersion ? "a229a74ebb9bac69327f33a4416986d614eda7ea" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflexPlatformVersion}.tar.gz";
in

(import reflex-platform {}).project ({ pkgs, ghc8_4, hackGet, ... }: 
with pkgs.haskell.lib;
{
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
          ] (name: (if !(self.ghc.isGhcjs or false) then pkgs.lib.id else dontCheck) super.${name});

      manualOverrides = self: super: {
        estuary = overrideCabal super.estuary (drv: {
          preConfigure = ''
          ${ghc8_4.hpack}/bin/hpack;
          '';
        });

        estuary-common = overrideCabal super.estuary-common (drv: {
          preConfigure = ''
          ${ghc8_4.hpack}/bin/hpack;
          '';
        });

        musicw = if !(self.ghc.isGhcjs or false) then null
          else dontHaddock 
            (self.callCabal2nix "musicw" (import ./deps/musicw) {});

        punctual = if !(self.ghc.isGhcjs or false) then null
          else self.callCabal2nix "punctual" (import ./deps/punctual) {};

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null
          else doJailbreak 
            (self.callCabal2nix "tidal" (import ./deps/tidal) {});

        # It is a nix package, but use cabal2nix anyways. The nix one 
        # has a bad base constraint.
        reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" (import ./deps/reflex-dom-contrib) {};
      };
    in
      pkgs.lib.composeExtensions skipBrokenGhcjsTests manualOverrides;
})
