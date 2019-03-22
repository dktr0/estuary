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
    ghc = ["estuary-common" "estuary-server"];
    ghcjs = ["estuary" "estuary-common"];
  };

  shellToolOverrides = ghc: super: {
    inherit (ghc8_4) hpack; # always use ghc (not ghcjs) compiled hpack
  };

  # A shell for staging and packaging releases
  passthru = {
    shells.release = pkgs.mkShell {
      buildInputs = (
        with pkgs;
        [closurecompiler gnumake gzip gcc]
      );
    };
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
          postInstall = ''
            ${pkgs.closurecompiler}/bin/closure-compiler $out/bin/Estuary.jsexe/all.js \
            --compilation_level=SIMPLE \
            --js_output_file=$out/bin/all.min.js \
            --externs=$out/bin/Estuary.jsexe/all.js.externs \
            --jscomp_off=checkVars
         '';
        });

        estuary-common = overrideCabal super.estuary-common (drv: {
          preConfigure = ''
            ${ghc8_4.hpack}/bin/hpack;
          '';
        });

        webdirt = import ./deps/webdirt self;

        musicw = if !(self.ghc.isGhcjs or false) then null
          else dontHaddock (import ./deps/musicw self);

        punctual = if !(self.ghc.isGhcjs or false) then null
          else import ./deps/punctual self;

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null
          else doJailbreak (import ./deps/tidal self);

        # It is a nix package, but use cabal2nix anyways. The nix one 
        # has a bad base constraint.
        reflex-dom-contrib = import ./deps/reflex-dom-contrib self;
      };
    in
      pkgs.lib.composeExtensions skipBrokenGhcjsTests manualOverrides;
})
