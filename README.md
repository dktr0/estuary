# estuary

projectional editing platform for Tidal

There are two parts to this, a server and a client. The server listens on a WebSocket for messages, and interprets the messages as Tidal patterns (which then normally results in sound coming from Dirt). The client presents a projectional editing interface for Tidal/Haskell expressions, sending these expressions to the server via a WebSocket. The server and client can be used separately if that is helpful.

## Server Instructions

1. Install GHC and cabal, if you don't already have it/them. There are several ways to do this.

2. Install dependencies for Tidal and dirt:
```
brew install liblo
brew install libsamplerate
brew install libsndfile
brew install jack
```

3. Clone and compile the Dirt sample playback engine (which Tidal uses to make sound):
```
git clone https://github.com/tidalcycles/Dirt.git
cd Dirt
make
```

4. Install the hint and tidal packages for Haskell (using cabal):
```
cabal install hint
cabal install tidal
```

5. Compile the server using GHC:
```
cd Estuary/server
ghc EstuaryServer.hs
```

6. Now you can run the jack audio routing system, Dirt, and the Estuary server.  Assuming Dirt and Estuary's source trees are both under your user directory it would be as follows (note that steps 1-5 above are install steps to be done once only):
```
jackd -d coreaudio &
cd ~/Dirt
./dirt &
cd ~/Estuary/server
./EstuaryServer &
```
Note: You can try out the server without the client: just use your web browser to open the file simpleWebSocketClient.html in the Estuary/server folder.

Another Note: to stop the server press Ctrl-Z, then kill the process by number. Ctrl-C won't work and if you kill the parent terminal without stopping the server and killing the process by number it seems the websocket port stays locked until you restart the machine. Here's an example:
```
pc-ogborn-d:server ogbornd$ ./EstuaryServer
Tidal websocket server for Estuary
^Z
[1]+  Stopped                 ./EstuaryServer
pc-ogborn-d:server ogbornd$ ps
  PID TTY           TIME CMD
  668 ttys000    0:00.08 -bash
  807 ttys000    0:00.02 ./EstuaryServer
pc-ogborn-d:server ogbornd$ kill 807
[1]+  Terminated: 15          ./EstuaryServer
pc-ogborn-d:server ogbornd$ 
```

## Client Instructions

We think the steps are like this, but make no guarantees:

1. install ghc, if you don't already have it. There are several ways to do this.

2. install stack on your system, and also some other dependencies. If you use homebrew on OS X, then this works:
```
brew install haskell-stack
brew install automake
brew install glib
brew install cairo
brew install pango
brew install gtk+
brew install gtk+3
brew install webkitgtk
```

3. install ghcjs on your system:
```
cabal install ...
ghcjs-boot --dev
```

4. run stack setup from the top level of the estuary folder to setup the project for building with ghcjs, then after that works, run stack compile to grab project dependencies (Haskell packages) and compile using ghcjs:
```
cd Estuary
stack setup
stack build
```

5. To open the editor, change to the build directory and open the index.html file. That is more complicated than it seems, here's a suggestion:
```
cd $(stack path --local-install-root)/bin/Estuary.jsexe
open index.html
```
