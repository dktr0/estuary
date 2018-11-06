STACK_CLIENT=cd client/ && stack
STACK_SERVER=cd server/ && stack
STACK_PRODUCTION_CLIENT=cd client/ && stack --work-dir .stack-work-production/

CLIENT_INSTALL_DIR=$$($(STACK_CLIENT) path --local-install-root)/bin/Estuary.jsexe
SERVER_INSTALL_DIR=$$($(STACK_SERVER) path --local-install-root)/bin/EstuaryServer
PRODUCTION_CLIENT_INSTALL_DIR=$$($(STACK_PRODUCTION_CLIENT) path --local-install-root)/bin/Estuary.jsexe

setupClient:
	$(STACK_CLIENT) setup

buildClient: setupClient
	$(STACK_CLIENT) build

EXTERNS=--externs=static/SuperDirt.js --externs=static/EstuaryProtocol.js --externs=static/WebDirt/WebDirt.js --externs=static/WebDirt/SampleBank.js --externs=static/WebDirt/Graph.js
CLOSURE_COMPILER="java -jar closure-compiler.jar"

prodBuildClient: setupClient
	$(STACK_PRODUCTION_CLIENT) build --ghc-options="-DGHCJS_BROWSER -O2"
	"$(CLOSURE_COMPILER)" "$(PRODUCTION_CLIENT_INSTALL_DIR)/all.js" --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --js_output_file="$(PRODUCTION_CLIENT_INSTALL_DIR)/all.min.js" $(EXTERNS)
	gzip -fk "$(PRODUCTION_CLIENT_INSTALL_DIR)/all.min.js"

buildClientForceDirty:
	$(STACK_CLIENT) build --force-dirty

setupServer:
	$(STACK_SERVER) setup

buildServer: setupServer
	$(STACK_SERVER) build

CLIENT_GCC_PREPROCESSOR=$(STACK_CLIENT) exec -- gcc -E -x c -P -C -nostdinc

installClient: buildClient
	cp -Rf $(CLIENT_INSTALL_DIR) .
	cp -Rf static/* Estuary.jsexe
	$(CLIENT_GCC_PREPROCESSOR) ../Estuary.jsexe/index.html.template -o ../Estuary.jsexe/index.html

prodInstallClient: # make prodBuildClient first!
	rm -rf ./Estuary.jsexe
	cp -Rf $(PRODUCTION_CLIENT_INSTALL_DIR) .
	cp -Rf static/* Estuary.jsexe
	$(CLIENT_GCC_PREPROCESSOR) ../Estuary.jsexe/index.html.template -DPRODUCTION -o ../Estuary.jsexe/index.html
	rm -rf Estuary.jsexe/runmain.js
	rm -rf Estuary.jsexe/rts.js
	rm -rf Estuary.jsexe/lib.js
	rm -rf Estuary.jsexe/out.js
	rm -rf Estuary.jsexe/all.js
	rm -rf Estuary.jsexe/out.stats
	rm -rf Estuary.jsexe/index.html.template

installServer: buildServer
	mkdir -p EstuaryServer
	cp $(SERVER_INSTALL_DIR) ./EstuaryServer/EstuaryServer

prodCleanBuildInstall: prodClean clean prodBuildClient buildServer prodInstallClient installServer

releaseClient: # make installClient or prodInstallClient first!
	rm -rf temp
	mkdir temp
	cp -Rf Estuary.jsexe temp
	rm -rf temp/Estuary.jsexe/Dirt
	# tar czf estuary-client.tgz -C temp .
	cd temp; zip -r ../estuary-client.zip ./*
	rm -rf temp

curlReleaseClient: # this uses curl to download and unzip a recent pre-built client from a GitHub release
	rm -rf Estuary.jsexe
	curl -o temp.zip -L https://github.com/d0kt0r0/estuary/releases/download/20181028/estuary-client-20181028.zip
	unzip temp.zip
	rm -rf temp.zip
	cp -Rf static/Dirt Estuary.jsexe

clean:
	rm -rf Estuary.jsexe
	rm -rf $$($(STACK_CLIENT) path --local-install-root)/bin
	rm -rf $$($(STACK_SERVER) path --local-install-root)/bin
	$(STACK_CLIENT) clean
	$(STACK_SERVER) clean

prodClean: clean
	$(STACK_PRODUCTION_CLIENT) clean

prodClean: clean
	stack --work-dir .stack-work-production/ clean --stack-yaml=client.yaml

style:
	cp static/classic.css Estuary.jsexe

test: installClient installServer
	EstuaryServer/EstuaryServer test

openClient: installClient
	open Estuary.jsexe/index.html
