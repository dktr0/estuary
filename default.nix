{
  reflexPlatformVersion ? "9e306f72ed0dbcdccce30a4ba0eb37aa03cf91e3",
  musl ? false,     # build with musl instead of glibc
  linkType ? null   # executable linking mode, null will build the closest to unconfigured for the current platform.
                    # 'static' will completely statically link everything.
                    # 'static-libs' will statically link the Haskell libs and dynamically link system. linux default.
                    # 'dynamic' will dynamically link everything. darwin default.
} @ args:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflexPlatformVersion}.tar.gz";
in

(import reflex-platform {
  nixpkgsFunc = args:
      let origPkgs = import "${reflex-platform}/nixpkgs" args;
      in if musl then origPkgs.pkgsMusl else origPkgs;
}).project ({ pkgs, ghc8_6, ... }:
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
    inherit (ghc8_6) hpack; # always use ghc (not ghcjs) compiled hpack
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
            "yaml" "hpack" "base-compat-batteries" "text-show" "modular-arithmetic"
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
        estuary = overrideCabal (appendConfigureFlags super.estuary ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"]) (drv: {
          preConfigure = ''
            ${ghc8_6.hpack}/bin/hpack --force;
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
            ${ghc8_6.hpack}/bin/hpack --force;
          '';
        });

       estuary-server =
          let configure-flags = map (opt: "--ghc-option=${opt}") (
              []
              ++ (if !pkgs.stdenv.isLinux then [] else ({
                  static = [ "-optl=-pthread" "-optl=-static" "-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
                      "-optl=-L${pkgs.zlib.static}/lib" "-optl=-L${pkgs.glibc.static}/lib"
                    ];
                  dynamic = [ "-dynamic" "-threaded"];
                }.${linkType} or [])
              ) ++ (if !pkgs.stdenv.isDarwin then [] else ({
                  dynamic = [ "-dynamic" "-threaded"];
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
                ${ghc8_6.hpack}/bin/hpack --force;
              '';
            }
        );

        text-show = dontCheck super.text-show;

        text-short = dontCheck super.text-short;

        criterion = dontCheck super.criterion;

        webdirt = import ./deps/webdirt self;

        timeNot = if !(self.ghc.isGhcjs or false) then null else dontHaddock
        #(self.callCabal2nix "timeNot" ../timeNot {});
        (self.callCabal2nix "TimeNot" (pkgs.fetchFromGitHub {
            owner = "afrancob";
            repo = "timeNot";
            sha256 = "0bij6wzb85kb9wbqqjdpb6x3s9w48kb62zxi2qw93p8jj15wyh84";
            rev =  "4da8daa5e1e16f6be3deba4637ad9aab0e46ee5e";
          }) {});

        punctual = dontHaddock (self.callCabal2nix "punctual" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "punctual";
          sha256 = "0d1jmwkrp4aj3v43j37w3jv1lb07clg15yc8vgpygjirfjcry7hw";
          rev = "e91567a43d852a189e5e5b50ace771792875686a";
        }) {});

        musicw = if !(self.ghc.isGhcjs or false) then null else dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "musicw";
          sha256 = "0rx7z6q2x1xqccwwc7036gzyy28kb8k45fj2v0zmjgsnv496qfd9";
          rev = "820f78d97e2257d5445f498f81f9e9a8b3d06e43";
        }) {});

        reflex-dom-contrib = if !(self.ghc.isGhcjs or false) then null else dontHaddock (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
          sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
        }) {});

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null else dontCheck (doJailbreak (self.callCabal2nixWithOptions "tidal"
          ( pkgs.fetchgit {
          url = "https://github.com/dktr0/Tidal.git";
          sha256 = "0r3nz4hxwvgwjka6rhkyhp6g0wbgjyaik0wkk7n27jp5mj5lwcv0";
          rev = "3e3f568c059912ee90f337fb4d21b56b88c4ada7";
          fetchSubmodules = true;
          }) "" {}));

        tidal-parse = if !(self.ghc.isGhcjs or false) then null else dontCheck (doJailbreak (self.callCabal2nixWithOptions
#            "tidal-parse" ../tidal/tidal-parse "" {});
            "tidal-parse"
            ( pkgs.fetchgit {
            url = "https://github.com/dktr0/Tidal.git";
            sha256 = "0r3nz4hxwvgwjka6rhkyhp6g0wbgjyaik0wkk7n27jp5mj5lwcv0";
            rev = "3e3f568c059912ee90f337fb4d21b56b88c4ada7";
            fetchSubmodules = true;
              })
            "--subpath tidal-parse" {}));

        wai-websockets = dontCheck super.wai-websockets; # apparently necessary on OS X

        haskellish = # dontHaddock (self.callCabal2nix "haskellish" ../Haskellish {}); #
        dontHaddock (self.callCabal2nix "haskellish" (pkgs.fetchFromGitHub {
           owner = "dktr0";
           repo = "Haskellish";
           sha256 = "05dn0lyyaagljibxf0pnlin9ni1c5v7kh9c4bs76ch9z5wsk9nf1";
           rev = "a1d78aedde218f8429517d8cfc599f9d7bbbc2d3";
        }) {});

        tempi = # dontHaddock (self.callCabal2nix "tempi" ../tempi {});
         dontHaddock (self.callCabal2nix "tempi" (pkgs.fetchFromGitHub {
           owner = "dktr0";
           repo = "tempi";
           sha256 = "10apprl5hzf7i7j6yp35vah174ds78x1q695cmcdcyd4qin3lzim";
           rev = "7e4ddcfd57af1d28c6970e9039701804e7145deb";
        }) {});

      };
    in
      pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
        skipBrokenGhcjsTests disableServerDependenciesOnGhcjs manualOverrides
      ];
})
