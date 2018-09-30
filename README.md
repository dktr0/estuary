# Four Ways to Start Using Estuary

- (Easiest) The completely zero installation route: At the time of writing, a stable, recent version of Estuary is online
  24/7 at a test server belonging to the research group at McMaster University that is working
  on the server - you can try it out anytime at the following URL (and if you have
  questions take them either to the &num;estuary channel on talk.lurk.org
  or the "estuary" Google group): http://intramuros.mcmaster.ca:8002

- (Easy) The almost zero installation route: For stand-alone usage (ie. solo live coding without networked collaboration), you
  can download the most recent compiled JavaScript "release" from github, unzip it, and add a sample library - no need to
  build anything. Just use your web browser to open the file index.html in your downloaded and unzipped
  release and you're good to go! Estuary expects the sample library to be a folder called samples under a folder called Dirt in your unzipped folder. You might download Dirt-Samples from the TidalCycles project (here https://github.com/tidalcycles/Dirt-Samples) and then put all of those sample folders in a folder called samples in a folder called Dirt in your unzipped Estuary folder, for example.

- (Somewhat easy) Building the server while using a pre-built JavaScript client: It is typically much easier to build the Estuary server
  than to build the Estuary client, so the Estuary Makefile provides targets for getting and incorporating a pre-built Estuary client into the Estuary project folder, alongside a server that you build yourself. If you don't have it already, install a recent version of the Haskell "stack" tool. The instructions below, executed in the Estuary folder/directory, show the complete process of building and installing the server, getting a copy of Dirt from the TidalCycles project (as one suitable choice for a sample library), grabbing the most recent released client without having to build it, then launching the complete and newly minted Estuary setup:

```
make installServer
cd static; git clone https://github.com/TidalCycles/Dirt.git; cd ..
cd static/Dirt; git submodule init; git submodule update; cd ../..
make curlReleaseClient
EstuaryServer/EstuaryServer someSecurePasswordYouPick
```

- (Difficult) If - despite the above three much easier and quicker options - you are for some reason still intent on building
  both Estuary's client and server "from scratch" on your machine, follow the detailed instructions below.

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
apt-get install ghc
apt-get install cabal-install
exit
```

(Note: some might ask why we need to install ghc and cabal-install when we will be using stack as our build tool.
The answer is that it seems that the version of ghcjs we are using requires that an older version of cabal-install
(version 1.24.0.1 is confirmed as working) be available on the path.)

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



## Building on Windows

To build on Windows you need to enable the Windows Subsystem for Linux. The following instructions pertain to a Debian installation on Windows

Enable Windows Subsystem for Linux  - run power shell as administrator and enter:

```Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux```

Disable legacy mode from command prompt: open a Command Prompt, right click on the header bar and click properties, uncheck legacy mode

Install Debian: You can install it directly from the Microsoft store.

Open Debian - initialize and set up a user (follow prompts)

Get latest apt packages in Debian terminal
```
sudo apt-get update
sudo apt-get upgrade
```


Install haskell platform, haskell stack, git, and curl for Debian

```
sudo apt-get install haskell-platform haskell-stack curl git
```

Install nodejs

```
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install -y nodejs
```

In a regular windows Command prompt (ie. not WSL) clone the Estuary repository:

```
git clone https://github.com/d0kt0r0/Estuary.git
```

Now back in the Debian terminal you should be able to find the cloned repository in: /mnt/c/<....path to cloned repo on windows...>

```
cd /mnt/c/ ...<path to cloned repo>
```

Follow the rest of the instructions under Linux detailed above after ```git clone https://gitub.com/d0kt0r0/Estuary.git``` in the Debian terminal.
