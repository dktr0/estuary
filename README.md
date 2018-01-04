# Building Estuary

First of all, note that, depending on your purpose and circumstances, you probably
don't need to build Estuary at all!

- At the time of writing, a stable, recent Estuary server is online 24/7 at a
  test server belonging to the research group at McMaster University that is working
  on the server - you can try it out anytime at the following URL (and if you have
  questions take them either to the &num;estuary channel on the live coding slack space
  or the "estuary" Google group): http://intramuros.mcmaster.ca:8002

- For stand-alone usage (ie. solo live coding without networked collaboration), you
  can download a built "release" from github, and add a sample library - no need to
  build anything. Just use your web browser to open the file index.html in your downloaded
  release and you're good to go!

If - despite the above much easier and quicker options - you're still intent on building
Estuary's client and server "from scratch" on your machine, the recommended method is via
the Haskell stack application, although other methods may be possible.

## Building on Debian 9.1 (Stretch)

These instructions have been tested against a fresh
installation of Debian 9.1. First, install binary
dependencies as root:

```
su
apt-get install git
apt-get install nodejs nodejs-legacy
apt-get install libncurses5-dev
apt-get install libghc-entropy-dev
apt-get install happy
apt-get install zlib1g-dev
apt-get install haskell-stack
exit
```

Now we need to upgrade stack, as the default package installed
on Debian is somewhat old. As your preferred (not root) user:

```
stack upgrade
```

You might very well get an error when you do this that suggests "rerunning with
--install-ghc" - if you do get that error, follow the suggestion like so:

```
stack upgrade --install-ghc
```

Note: If you encounter an error while doing stack upgrade, it's worth just trying again,
especially if you are doing this all on a computer that doesn't have much memory. If
you encounter an error that recurs through multiple re-attempts, feel free to bring
it up on the &num;estuary channel on the live coding slackspace, or the estuary
Google group.

Once stack upgrade has appeared to succeed, add $HOME/.local/bin to your PATH environment
variable as the warning at the end of the successful stack upgrade process suggests! You
may want to confirm you have version 1.5.1 or higher as follows:

```
stack --version
```

With those dependencies in place, here is what the rest of the process of building
and launching a complete Estuary installation might look like, starting from the
process of cloning the repository from github, and including downloading the Dirt
sampling library as one possible sample library to be used by WebDirt (Estuary's
sampling engine). Note: The "make buildClient" step below will probably take a *very*
long time (unless you happen to already have a working identical version of the ghcjs
compiler installed and booted via stack...):

```
cd ~
git clone https://github.com/d0kt0r0/Estuary.git
cd Estuary
git submodule init
git submodule update
cd static
git clone https://github.com/TidalCycles/Dirt.git
cd Dirt
git submodule init
git submodule update
cd ../..
make setupClient
make buildClient
make installClient
make setupServer
make buildServer
make installServer
./EstuaryServer/EstuaryServer somePassword 8002
```

The 8002 in the example above is a TCP port number and can be changed to
another suitable port as necessary. To use this newly built Estuary installation
you would open your web browser and point it to: 127.0.0.1:8002

The overall process is similar on OS X (we should add more detailed instructions soon...).
We have not been able to build the client on Windows so far, but the server yes. Windows
users can use a release of the client with their own build of the server.
