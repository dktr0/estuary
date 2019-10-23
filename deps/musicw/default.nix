{ callCabal2nix, ... }:
callCabal2nix "musicw" (
  import ../github-dep.nix {
    spec = ./github.json;
  }
) {}
