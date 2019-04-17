# If 'rsync' installed, use it to perform copies which only update if newer
# otherwise falling back to a plain 'cp'.
RSYNC_EXISTS := $(shell rsync --version 2>/dev/null)
ifdef RSYNC_EXISTS
CP_RECURSIVE=rsync --recursive --update --perms --executability
else
CP_RECURSIVE=cp -rf
endif

STACK_CLIENT=cd client/ && stack
STACK_SERVER=cd server/ && stack
STACK_PRODUCTION_CLIENT=cd client/ && stack --work-dir .stack-work-production/

CLIENT_INSTALL_DIR=$$($(STACK_CLIENT) path --local-install-root)/bin/Estuary.jsexe
SERVER_INSTALL_DIR=$$($(STACK_SERVER) path --local-install-root)/bin/EstuaryServer
PRODUCTION_CLIENT_INSTALL_DIR=$$($(STACK_PRODUCTION_CLIENT) path --local-install-root)/bin/Estuary.jsexe

setupClient:
	$(STACK_CLIENT) setup

buildClient: setupClient
	$(STACK_CLIENT) build estuary:exe:Estuary

EXTERNS=--externs=static/SuperDirt.js --externs=static/EstuaryProtocol.js --externs=static/WebDirt/WebDirt.js --externs=static/WebDirt/SampleBank.js --externs=static/WebDirt/Graph.js
CLOSURE_COMPILER="java -jar closure-compiler.jar"

prodBuildClient: setupClient
	$(STACK_PRODUCTION_CLIENT) build --ghc-options="-DGHCJS_BROWSER -O2 -dedupe" estuary:exe:Estuary
	"$(CLOSURE_COMPILER)" "$(PRODUCTION_CLIENT_INSTALL_DIR)/all.js" --compilation_level=SIMPLE --jscomp_off=checkVars --js_output_file="$(PRODUCTION_CLIENT_INSTALL_DIR)/all.min.js" $(EXTERNS)
	gzip -fk "$(PRODUCTION_CLIENT_INSTALL_DIR)/all.min.js"

prodBuildClientForceDirty: setupClient
	$(STACK_PRODUCTION_CLIENT) build --force-dirty --ghc-options="-DGHCJS_BROWSER -O2 -dedupe" estuary:exe:Estuary
	"$(CLOSURE_COMPILER)" "$(PRODUCTION_CLIENT_INSTALL_DIR)/all.js" --compilation_level=SIMPLE --jscomp_off=checkVars --js_output_file="$(PRODUCTION_CLIENT_INSTALL_DIR)/all.min.js" $(EXTERNS)
	gzip -fk "$(PRODUCTION_CLIENT_INSTALL_DIR)/all.min.js"

buildClientForceDirty:
	$(STACK_CLIENT) build --force-dirty estuary:exe:Estuary

setupServer:
	$(STACK_SERVER) setup

buildServer: setupServer
	$(STACK_SERVER) build

CLIENT_GCC_PREPROCESSOR=$(STACK_CLIENT) exec -- gcc -E -x c -P -C -nostdinc

installClient: buildClient
	$(CP_RECURSIVE) $(CLIENT_INSTALL_DIR) .
	$(CP_RECURSIVE) static/* Estuary.jsexe
	$(CLIENT_GCC_PREPROCESSOR) ../Estuary.jsexe/index.html.template -o ../Estuary.jsexe/index.html

prodInstallClient: # make prodBuildClient first!
	rm -rf ./Estuary.jsexe
	cp -Rf $(PRODUCTION_CLIENT_INSTALL_DIR) .
	$(CP_RECURSIVE) static/* Estuary.jsexe
	$(CLIENT_GCC_PREPROCESSOR) ../Estuary.jsexe/index.html.template -DPRODUCTION -o ../Estuary.jsexe/index.html
	rm -rf Estuary.jsexe/runmain.js
	rm -rf Estuary.jsexe/rts.js
	rm -rf Estuary.jsexe/lib.js
	rm -rf Estuary.jsexe/out.js
	rm -rf Estuary.jsexe/all.js
	rm -rf Estuary.jsexe/out.stats
	rm -rf Estuary.jsexe/index.html.template

installInteractionTestClient:
	$(STACK_CLIENT) build estuary:exe:interaction-test --flag estuary:build-test-executables
	$(CP_RECURSIVE) $$($(STACK_CLIENT) path --local-install-root)/bin/interaction-test.jsexe/* Estuary.jsexe
	$(CP_RECURSIVE) static/* Estuary.jsexe
	$(CLIENT_GCC_PREPROCESSOR) ../Estuary.jsexe/index.html.template -DTEST -o ../Estuary.jsexe/index.html

installServer: buildServer
	mkdir -p EstuaryServer
	cp $(SERVER_INSTALL_DIR) ./EstuaryServer/EstuaryServer

prodCleanBuildInstall: prodClean clean prodBuildClient buildServer prodInstallClient installServer

releaseClient: # make installClient or prodInstallClient first!
	rm -rf temp
	mkdir temp
	cp -Rf Estuary.jsexe temp
	rm -rf temp/Estuary.jsexe/samples
	# tar czf estuary-client.tgz -C temp .
	cd temp; zip -r ../estuary-client.zip ./*
	rm -rf temp

curlReleaseClient: # this uses curl to download and unzip a recent pre-built client from a GitHub release
	rm -rf Estuary.jsexe
	curl -o temp.zip -L https://github.com/dktr0/estuary/releases/download/20190311/estuary-client-20190311.zip
	unzip temp.zip
	rm -rf temp.zip
	cp -Rf static/samples Estuary.jsexe

downloadDirtSamples:
	cd static && git clone https://github.com/TidalCycles/Dirt-Samples.git --depth 1
	$(CP_RECURSIVE) static/Dirt-Samples/ static/samples/
	rm -rf static/Dirt-Samples/

makeSampleMap:
	cd static/samples && bash ../WebDirt/makeSampleMap.sh . > sampleMap.json

clean:
	rm -rf Estuary.jsexe
	rm -rf $$($(STACK_CLIENT) path --local-install-root)/bin
	rm -rf $$($(STACK_SERVER) path --local-install-root)/bin
	$(STACK_CLIENT) clean
	$(STACK_SERVER) clean

prodClean: clean
	$(STACK_PRODUCTION_CLIENT) clean

style:
	cp -r static/css-custom/ Estuary.jsexe
	cp -r static/css-source/ Estuary.jsexe

test: installClient installServer
	EstuaryServer/EstuaryServer test

buildTest: buildClient installClient
		EstuaryServer/EstuaryServer test

openClient: installClient
	open Estuary.jsexe/index.html
