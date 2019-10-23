{ callCabal2nix, ... }: 
callCabal2nix "reflex-dom-contrib" (
  import ../github-dep.nix {
    spec = ./github.json;
  }
) {}