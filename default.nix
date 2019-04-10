{ reflexPlatformVersion ? "7e002c573a3d7d3224eb2154ae55fc898e67d211" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflexPlatformVersion}.tar.gz";
in

(import reflex-platform {}).project ({ pkgs, ghc8_4, hackGet, ... }:
with pkgs.haskell.lib;
{
  name = "Estuary";

  packages =
    let filter = name: type:
      pkgs.lib.cleanSourceFilter name type && (
        let baseName = baseNameOf (toString name);
        in !(
          (type == "directory" && (pkgs.lib.hasPrefix ".stack" baseName || baseName == "node_modules"))
          || (type == "file" && (baseName == "stack.yaml" || pkgs.lib.hasSuffix ".cabal" baseName))
        )
      );
    in {
      estuary = pkgs.lib.cleanSourceWith {inherit filter; src = ./client; };
      estuary-common = pkgs.lib.cleanSourceWith {inherit filter; src = ./common; };
      estuary-server = pkgs.lib.cleanSourceWith {inherit filter; src = ./server; };
    };

  shells = {
    ghc = ["estuary-common" "estuary-server"];
    ghcjs = ["estuary" "estuary-common"];
  };

  shellToolOverrides = ghc: super: {
    inherit (ghc8_4) hpack; # always use ghc (not ghcjs) compiled hpack
    python3 = pkgs.python3.withPackages (ps: with ps; [ pyyaml ]);
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
      # a hacky way of avoiding building unnecessary dependencies with GHCJS
      # (our system is currently building GHC dependencies even for the front-end...
      # ...this gets around that to allow a build on OS X
      disableServerDependenciesOnGhcjs = self: super:
        pkgs.lib.genAttrs [
            "foundation" "memory" "wai-app-static" "asn1-types"
            "asn1-encoding" "asn1-parse" "sqlite-simple" "cryptonite"
            "http-client" "pem" "x509" "connection" "tls" "http-client-tls"
            "hpack"
          ] (name: if (self.ghc.isGhcjs or false) then null else super.${name});

      manualOverrides = self: super: {
        estuary = overrideCabal (appendConfigureFlags super.estuary ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe"]) (drv: {
          preConfigure = ''
            ${ghc8_4.hpack}/bin/hpack --force;
          '';
          postInstall = ''
            ${pkgs.closurecompiler}/bin/closure-compiler $out/bin/Estuary.jsexe/all.js \
            --compilation_level=SIMPLE \
            --js_output_file=$out/bin/all.min.js \
            --externs=$out/bin/Estuary.jsexe/all.js.externs \
            --jscomp_off=checkVars;
            gzip -fk "$out/bin/all.min.js"
         '';
        });

        estuary-common = overrideCabal (appendConfigureFlags super.estuary-common ["--ghc-options=-dynamic" "--ghc-options=-threaded"]) (drv: {
          preConfigure = ''
            ${ghc8_4.hpack}/bin/hpack --force;
          '';
        });

        estuary-server = overrideCabal (appendConfigureFlags super.estuary-server ["--ghc-options=-dynamic" "--ghc-options=-threaded"]) (drv: {
          preConfigure = ''
            ${ghc8_4.hpack}/bin/hpack --force;
          '';
          # based on fix from https://github.com/NixOS/nixpkgs/issues/26140
          preFixup = (drv.preFixup or "") + (
            if !pkgs.stdenv.isLinux
            then ""
            else ''
              NEW_RPATH=$(patchelf --print-rpath "$out/bin/EstuaryServer" | sed -re "s|/tmp/nix-build-estuary-server[^:]*:||g");
              patchelf --set-rpath "$NEW_PATH" "$out/bin/EstuaryServer";
            ''
          );
        });

        webdirt = import ./deps/webdirt self;

        musicw = if !(self.ghc.isGhcjs or false) then null
          else dontHaddock (import ./deps/musicw self);

        punctual = if !(self.ghc.isGhcjs or false) then null
          else import ./deps/punctual self;

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null
          else doJailbreak (import ./deps/tidal self);

        wai-websockets = dontCheck super.wai-websockets; # apparently necessary on OS X

        # It is a nix package, but use cabal2nix anyways. The nix one
        # has a bad base constraint.
        reflex-dom-contrib = import ./deps/reflex-dom-contrib self;
      };
    in
      pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
        skipBrokenGhcjsTests disableServerDependenciesOnGhcjs manualOverrides
      ];
})
