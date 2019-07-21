{
  reflexPlatformVersion ? "716879f16d53c93766e7ed9af17416fccb2edfe1",
  musl ? false,     # build with musl instead of glibc
  linkType ? null   # exectuable linking mode, null will build the closest to unconfigured for the current platform.
                    # 'static' will completely statcially link everything.
                    # 'static-libs' will statically link the haskell libs and dynamically link system. linux default.
                    # 'dynamic' will dynamically link everything. darwin default.
} @ args:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflexPlatformVersion}.tar.gz";
in

(import reflex-platform {
  nixpkgsFunc = args:
      let origPkgs = import "${reflex-platform}/nixpkgs" args;
      in if musl then origPkgs.pkgsMusl else origPkgs;
}).project ({ pkgs, ghc8_4, ... }:
with pkgs.haskell.lib;
let linkType = if (args.linkType or null) != null
  then args.linkType
  else
    if pkgs.stdenv.isDarwin then "static"
    else if pkgs.stdenv.isLinux then "static-libs"
    else "static-libs"; # fallback to static-libs which is closest to no intervention
in
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
      estuary = pkgs.lib.cleanSourceWith { inherit filter; src = ./client; };
      estuary-common = pkgs.lib.cleanSourceWith { inherit filter; src = ./common; };
      estuary-server = pkgs.lib.cleanSourceWith { inherit filter; src = ./server; };
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

        estuary-common = overrideCabal super.estuary-common (drv: {
          preConfigure = ''
            ${ghc8_4.hpack}/bin/hpack --force;
          '';
        });

       estuary-server =
          let configure-flags = map (opt: "--ghc-option=${opt}") (
              []
              ++ (if !pkgs.stdenv.isLinux then [] else ({
                  static = [ "-optl=-pthread" "-optl=-static" "-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
                      "-optl=-L${pkgs.zlib.static}/lib" "-optl=-L${pkgs.glibc.static}/lib"
                    ];
                  dynamic = [ "-dynamic" "-threaded" ];
                }.${linkType} or [])
              ) ++ (if !pkgs.stdenv.isDarwin then [] else ({
                  dynamic = [ "-dynamic" "-threaded" ];
                }.${linkType} or [])
              )
          );
          in
          overrideCabal (appendConfigureFlags super.estuary-server configure-flags) (drv:
            ({
              dynamic = {
                  # based on fix from https://github.com/NixOS/nixpkgs/issues/26140, on linux when building a dynamic exe
                  # we need to strip a bad reference to the temporary build folder from the rpath.
                  preFixup = (drv.preFixup or "") + (
                    if !pkgs.stdenv.isLinux
                    then ""
                    else ''
                      NEW_RPATH=$(patchelf --print-rpath "$out/bin/EstuaryServer" | sed -re "s|/tmp/nix-build-estuary-server[^:]*:||g");
                      patchelf --set-rpath "$NEW_PATH" "$out/bin/EstuaryServer";
                    ''
                  );
                };
              static = {
                  enableSharedExecutables = false;
                  enableSharedLibraries = false;
                };
            }.${linkType} or {}) // {
              preConfigure = ''
                ${ghc8_4.hpack}/bin/hpack --force;
              '';
            }
        );

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
