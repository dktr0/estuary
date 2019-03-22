{ callCabal2nix, ... }: 
callCabal2nix "tidal" (
  import ../github-dep.nix {
    spec = ./github.json;
  }
) {}