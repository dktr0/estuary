# Building Estuary

Note that, depending on your purpose, you may not need to
build Estuary. For stand alone usage, an already built 
release will suffice (although you may wish to add a sample
library). The recommended method of building Estuary's components
(client and server) is via the Haskell stack application, although
other methods may be possible. 

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
apt-get install haskell-stack
exit
```

Now we need to upgrade stack, as the default package installed
on Debian is somewhat old. As your preferred (not root) user:

```
stack upgrade
```

...then add $HOME/.local/bin to your PATH environment variable as
the warning at the end of the stack upgrade process suggests! You
may want to confirm you have version 1.5.1 as follows:

```
stack --version
```

With those dependencies in place, here is what the rest of the 
process of building and launching a complete Estuary installation might look like,
starting from the process of cloning the repository from github, and
including downloading the Dirt sampling library as one possible sample
library to be used by WebDirt (Estuary's sampling engine):

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
./EstuaryServer/EstuaryServer somePassword 8002
```

The 8002 in the example above is a TCP port number and can be changed to
another suitable port as necessary. To use this
newly built Estuary installation you would open your web browser and point 
it to: 127.0.0.1:8002

The overall process is similar on OS X (we should add more detailed instructions soon...). We have not been able to
build the client on Windows so far, but the server yes. Windows users can 
use a release of the client with their own build of the server.

