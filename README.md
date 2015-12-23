# estuary

projectional editing platform for Tidal

We think the steps are like this, but make no guarantees:

1. install stack on your system. If you use homebrew, then this works:
```
brew install haskell-stack
```

2. run stack setup from the top level of the estuary folder to setup an independent ghc installation and ghcjs. This will probably take a very long time:
```
cd estuary
stack setup
```

3. run stack build in the top level of the estuary folder to grab project dependencies and compile:
```
stack build
```

4. To open the editor, change to the build directory and open the index.html file. That is more complicated than it seems, here's a suggestion:
```
cd $(stack path --local-install-root)/bin/Estuary.jsexe
open index.html
```
