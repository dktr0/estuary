# Building Estuary

This document will outline how to build Estuary. It will walk you through:
*   the installation of the build tools
*   how to use the tools
*   how to setup a development environment
*   the relevant configuration files/project structure
*   how to update dependencies

**Important note**: In most cases you do *not* need to build Estuary. If you simply wish to try out Estuary and/or use it in many situations, it is simpler to use the Estuary server that is available 24/7 at https://intramuros.mcmaster.ca. Just fire up Chromium or Chrome and head to that address! Building Estuary is mostly something that is necessary only for the team working on developing Estuary itself.

**A note for Windows users**: nix is not supported natively on Windows but can be installed in WSL (Windows Subsystem for Linux). Follow the instructions on https://docs.microsoft.com/en-us/windows/wsl/install-win10 to install it. Then use it with any linux instructions from here on out. On occasion there are special notes for WSL users outlined in this document.

## Installing Nix

The build scripts are all based on [Nix](https://nixos.org/nix/) so the first step is to install Nix in whatever way is recommended/appropriate for your operating system (see the Nix website).

*   **Note:** On WSL your may get `warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)` forever if trying to install nix. See [NixOS/nix#1203](https://github.com/NixOS/nix/issues/1203) for the reason/fix for the issue. As suggested in that issue, setting `use-sqlite-wal = false` in the config (`/etc/nix/nix.conf`) does the trick!

    ```bash
    sudo mkdir /etc/nix; echo 'use-sqlite-wal = false' | sudo tee -a /etc/nix/nix.conf
    ```

Answer `1` for `Yes` if it asks to add binary caches to the config.

## Cloning Estuary

Use git to clone a copy of the Estuary repository. You can the central repository (maintained by dktr0) or you could fork it on github and clone your copy of the Estuary repository. (Note: do not use github's "download" feature to grab a copy of the Estuary source code - it will give you an Estuary folder that is not an active git project and you will be unable to build Estuary.)

```
git clone https://github.com/dktr0/Estuary
```

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

### Creating and launching a full local deployment of the Estuary server

```shell
$ make fullBuild
```

The full build process above will take a long time. After it completes successfully, a complete Estuary deployment will be present in the staging folder. To generate temporary SSL certificates for this deployment:

```shell
$ make selfCertificates
```

You usually need to enter a country code when making the self-signed certificates, but can leave the other fields blank.

If all goes well, you can run the server:

```shell
$ cd staging
$ ./EstuaryServer password 8000
```

The above launches a server that is listening on port 8000 for HTTPS requests, with 'password' as the administrative password. So you would connect to it in your web browser (on the same machine) with https://127.0.0.1:8000

Note: Typically if you want to launch the server on the "standard" HTTPS port (443) you will need root privileges. So that might look like this:

```shell
$ cd staging
$ sudo ./EstuaryServer password 443
```

## Building for development

The full build above always rebuilds everything from scratch. This is great for having predictable, deterministic results, but is very slow. When actively developing/changing Estuary, the workflow that follows is likely  preferable.

It is recommended to have 2 shells open. One for building and staging the client, and another for the server. The staging folder for the development commands is `dev-staging/`.

Run `nix-shell -A shells.ghcjs` in one terminal and `nix-shell -A shells.ghc` in the other.

In the **frontend** shell (`shells.ghcjs`) build the client and put it where the `runDevServer` expects it to be with:
```shell
[nix-shell: ...]$ make cabalBuildClient
[nix-shell: ...]$ make cabalStageClient
```

In the **backend** shell (`shells.ghc`) build the server, put it in the staging area, and run it with:
```shell
[nix-shell: ...]$ make cabalBuildServer
[nix-shell: ...]$ make cabalStageServer
[nix-shell: ...]$ make devStageSamples
[nix-shell: ...]$ make runDevServer
```
