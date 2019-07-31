{ callCabal2nix, ... }: 
callCabal2nix "TimeNot" (
  import ../github-dep.nix {
    spec = ./github.json;
  }
) {}
