# If 'rsync' installed, use it to perform copies which only update if newer
# otherwise falling back to a plain 'cp'.
RSYNC_EXISTS := $(shell rsync --version 2>/dev/null)
#ifdef RSYNC_EXISTS
#CP=rsync  --perms --executability
#CP_RECURSIVE=rsync --recursive --perms --executability
#else
CP=cp
CP_RECURSIVE=cp -Rf
#endif
WEBDIRT = $(shell nix-store -r $(shell nix-instantiate webdirt.nix))
HYDRA = $(shell nix-store -r $(shell nix-instantiate hydra-synth.nix))
PEGJS = $(shell nix-store -r $(shell nix-instantiate pegjs.nix))

# the hack below is necessary because cabal on OS x seems to build in a
# subdirectory name ...../x86_64-osx/... rather than the name in $system
ifeq (${system},x86_64-darwin)
	SYSTEM = x86_64-osx
else
	SYSTEM = ${system}
endif

assertInNixShell:
ifndef IN_NIX_SHELL
	$(error Must be run in a nix shell)
endif

assertInNixGhcjsShell:
ifndef NIX_GHCJS
	$(error Must be run in the nix ghcjs shell. "nix-shell -A shells.ghcjs")
endif

assertInNixGhcShell:
ifndef NIX_GHC
	$(error Must be run in the nix ghc shell. "nix-shell -A shells.ghc")
endif

cabalBuildClient: assertInNixGhcjsShell
	@ echo "cabalBuildClient:"
	cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all --disable-library-profiling --disable-documentation --ghcjs-options=-DGHCJS_GC_INTERVAL=60000

cabalBuildServer: assertInNixGhcShell
	@ echo "cabalBuildServer:"
	cabal new-build all --disable-library-profiling --disable-documentation

nixBuild:
	@ echo "nixBuild:"
	rm -rf result
	-mkdir result
	-mkdir result/ghc
	-mkdir result/ghcjs
	nix-build --max-jobs 4 --cores 0 -o result/ghc/estuary-server/ -A ghc.estuary-server
	nix-build --max-jobs 4 --cores 0 -o result/ghcjs/estuary/ -A ghcjs.estuary

nixBuildServer:
	@ echo "nixBuildServer:"
	rm -rf result
	-mkdir result
	-mkdir result/ghc
	-mkdir result/ghcjs
	nix-build --max-jobs 4 --cores 0 -o result/ghc/estuary-server/ -A ghc.estuary-server

stackBuildServer:
	@ echo "stackBuildServer:"
	cd server && stack setup
	cd server && stack build

stackStageServer: prepStage
	@ echo "stackStageServer:"
	cp -f $(shell cd server && stack path --local-install-root)/bin/EstuaryServer $(STAGING_ROOT)
	chmod a+w $(STAGING_ROOT)/EstuaryServer

PROD_STAGING_ROOT=staging
DEV_STAGING_ROOT=dev-staging
STAGING_ROOT=$(PROD_STAGING_ROOT)
prepStage:
	@ echo "prepStage:"
	-mkdir $(STAGING_ROOT)
	-mkdir $(STAGING_ROOT)/Estuary.jsexe/
	-mkdir $(STAGING_ROOT)/Estuary.jsexe/WebDirt
prepDevStage: STAGING_ROOT=$(DEV_STAGING_ROOT)
prepDevStage: prepStage

cleanStage:
	@ echo "cleanStage:"
	-rm -rf $(STAGING_ROOT)
cleanDevStage: STAGING_ROOT=$(DEV_STAGING_ROOT)
cleanDevStage: cleanStage

stageStaticAssets: prepStage
	@ echo "stageStaticAssets:"
	cp -Rf $(WEBDIRT)/dist/* $(STAGING_ROOT)/Estuary.jsexe/WebDirt
	cp -f $(HYDRA)/dist/hydra-synth.js $(STAGING_ROOT)/Estuary.jsexe/
	cp -f $(PEGJS)/* $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/*.js $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/css-custom $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/css-source $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/fonts $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/icons $(STAGING_ROOT)/Estuary.jsexe/

devStageStaticAssets: STAGING_ROOT=$(DEV_STAGING_ROOT)
devStageStaticAssets: stageStaticAssets

stageSamples: prepStage
	@ echo "stageSamples:"
	$(CP_RECURSIVE) static/samples $(STAGING_ROOT)/Estuary.jsexe/
devStageSamples: STAGING_ROOT=$(DEV_STAGING_ROOT)
devStageSamples: stageSamples

GCC_PREPROCESSOR=gcc -E -x c -P -C -nostdinc
TEMPLATE_SOURCE=static/index.html.template

GET_CABAL_CLIENT_PACKAGE_NAME=estuary-0.0.0.1
GET_GHCJS_VERSION=ghcjs --version | sed -nre "s/.*version ([^ ]*).*/\1/p"
CABAL_CLIENT_BIN_DIR=dist-ghcjs/build/x86_64-linux/ghcjs-${GHCJS_VERSION}/${CABAL_CLIENT_PACKAGE_NAME}/x/Estuary/build/Estuary/Estuary.jsexe/
cabalStageClient: assertInNixGhcjsShell prepDevStage
	@ echo "cabalStageClient:"
	$(eval export CABAL_CLIENT_PACKAGE_NAME=$(GET_CABAL_CLIENT_PACKAGE_NAME))
	$(eval export GHCJS_VERSION=$(shell $(GET_GHCJS_VERSION)))
	# compile the index.html template in development mode and stage it
	$(GCC_PREPROCESSOR) $(TEMPLATE_SOURCE) -o $(DEV_STAGING_ROOT)/Estuary.jsexe/index.html
	# stage the client js
	for part in lib out rts runmain ; do \
		$(CP) $(CABAL_CLIENT_BIN_DIR)/$$part.js $(DEV_STAGING_ROOT)/Estuary.jsexe/ ; \
		chmod a+w $(DEV_STAGING_ROOT)/Estuary.jsexe/$$part.js ; \
	done

GET_CABAL_SERVER_PACKAGE_NAME=estuary-server-0.0.0.1
GET_GHC_VERSION=ghc --version | sed -nre "s/.*version ([^ ]*).*/\1/p"
CABAL_SERVER_BIN=dist-newstyle/build/$(SYSTEM)/ghc-${GHC_VERSION}/${CABAL_SERVER_PACKAGE_NAME}/x/EstuaryServer/build/EstuaryServer/EstuaryServer
cabalStageServer: assertInNixGhcShell
	@ echo "cabalStageServer:"
	$(eval export CABAL_SERVER_PACKAGE_NAME=$(GET_CABAL_SERVER_PACKAGE_NAME))
	$(eval export GHC_VERSION=$(shell $(GET_GHC_VERSION)))
	# stage the server binary
	$(CP) $(CABAL_SERVER_BIN) $(DEV_STAGING_ROOT)
	chmod a+w $(DEV_STAGING_ROOT)/EstuaryServer

nixStageClient: prepStage
	@ echo "nixStageClient:"
	# compile the index.html template in production mode and stage it
	$(GCC_PREPROCESSOR) $(TEMPLATE_SOURCE) -DPRODUCTION -o $(STAGING_ROOT)/Estuary.jsexe/index.html
	# stage the minified client
	$(CP) result/ghcjs/estuary/bin/all.min.js $(STAGING_ROOT)/Estuary.jsexe/
	chmod a+w $(STAGING_ROOT)/Estuary.jsexe/all.min.js
	$(CP) result/ghcjs/estuary/bin/all.min.js.gz $(STAGING_ROOT)/Estuary.jsexe/
	chmod a+w $(STAGING_ROOT)/Estuary.jsexe/all.min.js.gz
nixDevStageClient: STAGING_ROOT=$(DEV_STAGING_ROOT)
nixDevStageClient: nixStageClient

nixStageServer: prepStage
	# stage the server binary
	@ echo "nixStageServer:"
	$(CP) result/ghc/estuary-server/bin/EstuaryServer $(STAGING_ROOT)
	chmod a+w $(STAGING_ROOT)/EstuaryServer
nixDevStageServer: STAGING_ROOT=$(DEV_STAGING_ROOT)
nixDevStageServer: nixStageServer

bundleClient: cleanStage stageStaticAssets nixStageClient
	(cd $(STAGING_ROOT) && zip -r - ./Estuary.jsexe/*) > estuary-client.zip

downloadDirtSamples:
	@ echo "downloadDirtSamples:"
	cd static && git clone https://github.com/TidalCycles/Dirt-Samples.git --depth 1
	-mkdir static/samples
	cd static/Dirt-Samples && cp -Rf * ../samples/
	rm -rf static/Dirt-Samples/
	@[ -d static/samples/bd ] || (echo "Error: make downloadDirtSamples did NOT work!" && exit 1)
	@ echo " Dirt samples downloaded."

generateDefaultAudioResources:
	@ echo "generateDefaultAudioResources:"
	@[ -d static/samples ] || (echo Directory static/samples does not exist. Have you provided a sample library, for example, by running 'make downloadDirtSamples'? && exit 1)
	cd static/samples && bash ../../generateAudioResources.sh . > resources.json
	@[ -f static/samples/resources.json ] || (echo "Error: generateDefaultAudioResources did NOT work!" && exit 1)
	@ echo " JSON default audio resources file generated."

fullBuild: downloadDirtSamples generateDefaultAudioResources nixBuild cleanStage nixStageClient nixStageServer stageStaticAssets stageSamples

clean:
	-rm -rf staging
	-rm -rf dev-staging
	-rm -rf result
	-rm -rf dist-newstyle
	-rm -rf dist-ghcjs

runDevServer: STAGING_ROOT=$(DEV_STAGING_ROOT)
runDevServer: stageStaticAssets stageSamples cabalBuildServer cabalStageServer
	cd ./$(STAGING_ROOT) && sudo ./EstuaryServer moderator community +RTS -N -RTS

runServer: nixBuild stageStaticAssets generateDefaultAudioResources stageSamples nixStageClient nixStageServer
	cd ./$(STAGING_ROOT) && sudo ./EstuaryServer moderator community +RTS -N -RTS

selfCertificates:
	openssl genrsa -out privkey.pem 2048
	openssl req -new -key privkey.pem -out cert.csr
	# -addext extendedKeyUsage=serverAuth -addext subjectAltName=DNS:localhost
	openssl x509 -req -in cert.csr -signkey privkey.pem -out cert.pem

stageLocalWebDirt: prepStage prepDevStage
	cp -Rf ~/WebDirt/dist/* $(STAGING_ROOT)/Estuary.jsexe/WebDirt/
	cp -Rf ~/WebDirt/dist/* $(DEV_STAGING_ROOT)/Estuary.jsexe/WebDirt/

clientTest:
	@ echo "clientTest:"
	cabal --ghcjs new-test --project-file=cabal-ghcjs.project --builddir=test-ghcjs test:clientTest --disable-library-profiling --disable-documentation

fetchPEGjs:
	$(PEGJS)
