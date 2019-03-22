{ callCabal2nix, ... }: 
callCabal2nix "punctual" (
  import ../github-dep.nix {
    spec = ./github.json;
  }
) {}