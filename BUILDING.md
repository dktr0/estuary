# Building Estuary

This document will outline how to build Estuary. It will walk you through:
*   the installation of the build tools
*   how to use the tools
*   how to setup a development environment
*   the relevant configuration files/project structure
*   how to update dependencies

**A note for Windows users**: nix is not supported natively on Windows but can be installed in WSL (Windows Subsystem for Linux). Follow the instructions on https://docs.microsoft.com/en-us/windows/wsl/install-win10 to install it. Then use it with any linux instructions from here on out. On occasion there are special notes for WSL users outlined in this document.

## Installing Nix via reflex-platform

The build scripts are all based on [Nix](https://nixos.org/nix/) so the first step is to install Nix via the `try-reflex` script.

```shell
# download the version used while writing this document
curl -L https://github.com/reflex-frp/reflex-platform/archive/a229a74ebb9bac69327f33a4416986d614eda7ea.tar.gz -o reflex-platform.tar.gz

# decompress the archive
mkdir reflex-platform/ && tar xf reflex-platform.tar.gz -C reflex-platform/ --strip-components 1
```

Next, run the `try-reflex` script at least once. This will install nix and setup the reflex package cache (this is very desireable for decreasing build times).

*   **Note:** On WSL your may get `warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)` forever if trying to install nix. See [NixOS/nix#1203](https://github.com/NixOS/nix/issues/1203) for the reason/fix for the issue. As suggested in that issue, setting `use-sqlite-wal = false` in the config (`/etc/nix/nix.conf`) does the trick!

    ```bash
    sudo mkdir /etc/nix; echo 'use-sqlite-wal = false' | sudo tee -a /etc/nix/nix.conf
    ```

Answer `1` for `Yes` if it asks to add binary caches to the config.

```shell
./reflex-platform/try-reflex
```

Unless `/nix` was already created on your system, running the above command would have installed Nix for you (it logs `performing a single-user installation of Nix...` during execution). 

```
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type

  . /home/<yourUsername>/.nix-profile/etc/profile.d/nix.sh

in your shell.
```

When things are ready done you will be in a nix-shell. You can tell because your prompt will be prefixed with `[nix-shell:<yourCurrentPath>]$`.

Get out of the shell with `exit` since we don't need to work in the shell right now. Also Estuary has it's own shell.

Lastly, double check that  `which nix-build` returns a path to the installation. If it doesn't, we need to set up the environment variables. Your `~/.bash_profile`, `~/.bash_login`, or `~/.profile` should have been modified by the installation to have something along the lines of the following at the end. If it does not, then add it so that when you log in:
```bash
if [ -e /home/<yourUsername>/.nix-profile/etc/profile.d/nix.sh ]; then . /home/<yourUsername>/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
```

Log out and log back in after the first install and check `which nix-build`.

## Building for a release

`nix-build` will build all the packages and symlink the output in the `result` folder. The `estuary-common` package is built with both shells.

```shell
tree result
result
├── ghc
│   ├── estuary-common -> /nix/store/...-estuary-common-0.0.0.1
│   └── estuary-server -> /nix/store/...-estuary-server-0.0.0.1
└── ghcjs
    ├── estuary -> /nix/store/...-estuary-0.0.0.1
    └── estuary-common -> /nix/store/...-estuary-common-0.0.0.1
``` 

The server binary is located at `result/ghc/estuary-server/bin/EstuaryServer`. 

The client `jsexe` is located at `result/ghcjs/estuary/bin/Estuary.jsexe/`. 

### Creating a release bundle

```shell
$ make bundleClient
```

The `bundleClient` target will create an `estuary-client.zip` with the production version of the client, front-end dependencies, and static assets (excluding samples).

### Creating a full local deployment

```shell
$ git submodule update --init --recursive
$ make downloadDirtSamples 
$ make makeSampleMap
$ make nixBuild
$ make cleanStage
$ make stageStaticAssets
$ make stageSamples
$ make nixStageClient
$ make nixStageServer
```

Running `make downloadDirtSamples makeSampleMap` only needs to be run once to use the dirt samples in the deployment.

The `cleanStage` will clean any old `staging/` after which the remaining commands will repopulate.

The `stage*` targets copy the required assets into the `staging/` folder.

The `nix*` targets will build and stage the server binary and client with `nix`. 

## Building for development

It is recommended to have 2 shells open. One for building and staging the client, and another for the server. The staging folder for the development commands is `dev-staging/`.

Run `nix-shell -A shells.ghcjs` in one terminal and `nix-shell -A shells.ghc` in the other.

In the **frontend** shell (`shells.ghcjs`) build the client and put it where the `runDevServer` expects it to be with:
```shell
[nix-shell: ...]$ make cabalBuildClient cabalStageClient
```

In the **backend** shell (`shells.ghc`) build the server, put it in the staging area, and run it with:
```shell
[nix-shell: ...]$ make runDevServer
```
