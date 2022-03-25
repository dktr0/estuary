{
  reflexPlatformVersion ? "123a6f487ca954fd983f6d4cd6b2a69d4c463d10",
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
            "yaml" "base-compat-batteries" "text-show" "modular-arithmetic"
          ] (name: (if !(self.ghc.isGhcjs or false) then pkgs.lib.id else dontCheck) super.${name});
      # a hacky way of avoiding building unnecessary dependencies with GHCJS
      # (our system is currently building GHC dependencies even for the front-end...
      # ...this gets around that to allow a build on OS X
      disableServerDependenciesOnGhcjs = self: super:
        pkgs.lib.genAttrs [
            "foundation" "memory" "wai-app-static" "asn1-types"
            "asn1-encoding" "asn1-parse" "sqlite-simple" "cryptonite"
            "http-client" "pem" "x509" "connection" "tls" "http-client-tls"
          ] (name: if (self.ghc.isGhcjs or false) then null else super.${name});

      manualOverrides = self: super: {
        estuary = dontCheck (overrideCabal (appendConfigureFlags super.estuary ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"]) (drv: {
          preConfigure = ''
          '';
          postInstall = ''
            ${pkgs.closurecompiler}/bin/closure-compiler $out/bin/Estuary.jsexe/all.js \
              --compilation_level=SIMPLE \
              --js_output_file=$out/bin/all.min.js \
              --externs=$out/bin/Estuary.jsexe/all.js.externs \
              --jscomp_off=checkVars;
            gzip -fk "$out/bin/all.min.js"
         '';
        }));

        estuary-common = overrideCabal super.estuary-common (drv: {
          preConfigure = ''
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
              '';
            }
        );

        webdirt = import ./deps/webdirt self;

        timeNot = if !(self.ghc.isGhcjs or false) then null else dontHaddock
        #(self.callCabal2nix "timeNot" ../timeNot {});
        (self.callCabal2nix "TimeNot" (pkgs.fetchFromGitHub {
            owner = "afrancob";
            repo = "timeNot";
            sha256 = "1jq2gwszdcw6blgcfj60knpj0pj6yc330dwrvjzq8ys8rp17skvq";
            rev =  "c6c88bd9003afc0c69a651f4156a34e6593044bf";
          }) {});

        punctual = # dontCheck (dontHaddock (self.callCabal2nix "punctual" ../Punctual {}));
          dontCheck (dontHaddock (self.callCabal2nix "punctual" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "punctual";
          sha256 = "1jszc80pv1m56haw75xrfm6qpkw267a0jgzr8821qpwbbwn5jgxl";
          rev = "dc863728d259aa840fe68c9536597e9087f09376";
        }) {}));

        # musicw = self.callHackage "musicw" "0.3.9" {};
        musicw = self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "musicw";
          sha256 = "19c31jnsdhw85qczgwcmfch217nfc631qv9jf4wzbyg0xz50s64c";
          rev = "b42ff7b1ea3ea322fb830a4e4757626761aebcd5";
        }) {};

        # needs jailbreak for dependency microspec >=0.2.0.1
        tidal = if !(self.ghc.isGhcjs or false) then null else dontCheck (doJailbreak (self.callCabal2nixWithOptions
        #  "tidal" ../Tidal "" {}));
          "tidal"
          ( pkgs.fetchgit {
          url = "https://github.com/dktr0/Tidal.git";
          sha256 = "1fx0la5i793fayrm073fkcsi2hmb42adfgc1nvdbivkkfl83w8ld";
          rev = "fbcd38a3e28ca160f023128690d6eee1da595b93";
          fetchSubmodules = true;
        }) "" {}));

        tidal-parse = if !(self.ghc.isGhcjs or false) then null else dontCheck (doJailbreak (self.callCabal2nixWithOptions
        #  "tidal-parse" ../Tidal/tidal-parse "" {}));
            "tidal-parse"
            ( pkgs.fetchgit {
            url = "https://github.com/dktr0/Tidal.git";
            sha256 = "1fx0la5i793fayrm073fkcsi2hmb42adfgc1nvdbivkkfl83w8ld";
            rev = "fbcd38a3e28ca160f023128690d6eee1da595b93";
            fetchSubmodules = true;
        })
        "--subpath tidal-parse" {}));

        wai-websockets = dontCheck super.wai-websockets; # apparently necessary on OS X

        haskellish = self.callCabal2nix "haskellish" (pkgs.fetchFromGitHub {
          owner = "dktr0";
          repo = "haskellish";
          sha256 = "03sv69hnav1p8rd6i301kirx4anm5f402is4n7bxmjjqi7br5hna";
          rev = "75cd924f8699da352ef4d441f35a18ee53d598b0";
        }) {};

        tempi = self.callHackage "tempi" "1.0.2.1" {};

        # we will won't be able to build this until reflex-dom-contrib dependency is removed
        #seis8s = #dontHaddock (self.callCabal2nix "seis8s" ../seis8s {});
        #  doJailbreak (dontHaddock (self.callCabal2nix "seis8s" (pkgs.fetchFromGitHub {
        #   owner = "luisnavarrodelangel";
        #   repo = "seis8s";
        #   sha256 = "1nnzsmkcy28k1s1s72ckq136564r2d6xzngm2bd1sm5ixasxx0lq";
        #   rev = "6edbf1e21ade2669a0098d3120c698463c86f52a";
         #}) {}));

         permutation = markUnbroken super.permutation;

         hosc = self.callHackage "hosc" "0.19.1" {};
         hsc3 = self.callHackage "hsc3" "0.19.1" {};
         hmt = doJailbreak (markUnbroken super.hmt);

         network = if !(self.ghc.isGhcjs or false) then super.network else
           let unpatchedNetwork = super.callHackageDirect {
             pkg = "network";
             ver = "3.1.2.7";
             sha256 = "1762r7jckinwvz5m99l5jr1p2p2d10jysg159nwlqxmsnr39waz7"; # note: temporarily set to pkgs.lib.fakeSha256 to find new hash...
           } { };
           in appendConfigureFlags unpatchedNetwork ["--configure-option=--host=x86_64-pc-linux-gnu"];

      };
    in
      pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
        skipBrokenGhcjsTests disableServerDependenciesOnGhcjs manualOverrides
      ];
})
