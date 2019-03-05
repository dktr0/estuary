## One time project setup

Based on a combination of `https://github.com/ElvishJerricco/reflex-project-skeleton` and `https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md`.

1.  Add `reflex-platform` as a git submodule:
    
    ```bash
    git submodule add https://github.com/reflex-frp/reflex-platform reflex-platform
    ```

    The `.gitmodules` should now contain:
    ```
    [submodule "reflex-platform"]
      path = reflex-platform
      url = https://github.com/reflex-frp/reflex-platform
    ```

## One time user setup

Install and setup nix via the reflex script:

**Note:** On WSL your may get `warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)` forever if trying to install nix. See [NixOS/nix#1203](https://github.com/NixOS/nix/issues/1203) for the reason/fix for the issue. As suggested in that issue, setting `use-sqlite-wal = false` in the config (`/etc/nix/nix.conf`) does the trick!

```bash
sudo mkdir /etc/nix; echo 'use-sqlite-wal = false' | sudo tee -a /etc/nix/nix.conf
```

Answer `1` for `Yes` if it asks to add binary caches to the config.

```bash
reflex-platform/try-reflex
```

Unless `/nix` was already created on your system, running the above command would have installed Nix for you (logs `performing a single-user installation of Nix...` during execution). 

```
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type

  . /home/<yourUsername>/.nix-profile/etc/profile.d/nix.sh

in your shell.
```

This will put you in the `nix-shell` after waiting for about 30mins for tool builds/cache downloads. Your prompt should be prefixed with `[nix-shell:<yourCurrentPath>]$`. Congrats, all installed!

Get out of the shell with `exit` since we don't need to work in the shell.

If after exiting the shell `which nix-build` does nothing, we need to set up the environment variables. Your `~/.bash_profile`, `~/.bash_login`, or `~/.profile` should have been modified by the installation to have something along the lines of the following at the end. If it does not, then add it so that when you log in, nix is made available outside of the `try-reflex` shell:
```bash
if [ -e /home/<yourUsername>/.nix-profile/etc/profile.d/nix.sh ]; then . /home/<yourUsername>/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
```

Log out and log back in after the first install and check `which nix-build`.

## Every time build

`nix-build` will build based on the `default.nix` configuration. That configuration extends the `reflex-platform/project/default.nix`.

A `nix-build` will run a full, reproducible, and consistent build. It takes a bit longer but should be used before a commit or release. The cabal builds are for getting quicker feedback during development.


## `apt-get` is replaced with `nix-env`

https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure

## Adding github dependencies

In the `default.nix`, using the `overrides` and `pkgs.fetchFromGitHub` we can build a cabal package from github:

```nix
  overrides = self: super: {
    musicw = self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "MusicW";
      rev = "94830ba9f5929abbd09094bc5d21d44c6a88a889";
      sha256 = "04k73rx2w04jn5lx8q7b2kjl4yqypvacv4v97bvrggg03kjvgbks";
    }) {};
  }
```

The `owner` and `repo` fields should be straight forward. The `rev` is a commit hash. The `sha256` field can be obtained via the `nix-prefetch-url` command:
```bash
$> nix-prefetch-url --unpack https://github.com/<owner>/<repo>/archive/<rev>.tar.gz

unpacking...
path is '/nix/store/...'
04k73rx2w04jn5lx8q7b2kjl4yqypvacv4v97bvrggg03kjvgbks
```

That last line is the `sha256`.

### Project structure

The `cabal.project` allows an incremental build with multiple local packages. This means we can work on `common` and `client` at the same time for example.

Haskell dependency management:
https://github.com/Gabriel439/haskell-nix/blob/master/project4/README.md

*   **jailbreak**: when applied to a haskell package means ignore the version constraints on it.


From https://gitlab.com/mightybyte/hexplore/blob/master/default.nix
```nix
{ rpRef ? "ea3c9a1536a987916502701fb6d319a880fdec96" }:

let rp = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";

in

(import rp {}).project ({ pkgs, ... }:
with pkgs.haskell.lib;
{
```

### LTS package sets

https://github.com/reflex-frp/reflex-platform/blob/develop/nixpkgs/github.json
The above holds the `nixpkgs` pin which in turn contains a currated package set of haskell packages that build together. As of writing this, that set is https://raw.githubusercontent.com/NixOS/nixpkgs/4507926b80c6b8f73053775ffee17f6781c7e7c8/pkgs/development/haskell-modules/hackage-packages.nix. 

This means that the version of `reflex-project` used **determines the base package set**!