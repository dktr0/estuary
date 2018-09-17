setupClient:
	stack setup --stack-yaml=client.yaml

buildClient: setupClient
	stack build --stack-yaml=client.yaml

PRODUCTION_INSTALL_DIR=$$(stack --work-dir .stack-work-production/ --stack-yaml=client.yaml path --local-install-root)/bin/Estuary.jsexe
EXTERNS=--externs=static/SuperDirt.js --externs=static/EstuaryProtocol.js --externs=static/WebDirt/WebDirt.js --externs=static/WebDirt/SampleBank.js --externs=static/WebDirt/Graph.js
CLOSURE_COMPILER="java -jar closure-compiler.jar"

prodBuildClient: setupClient
	stack --work-dir .stack-work-production/ build --stack-yaml=client.yaml --ghc-options="-DGHCJS_BROWSER -O2"
	"$(CLOSURE_COMPILER)" "$(PRODUCTION_INSTALL_DIR)/all.js" --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --js_output_file="$(PRODUCTION_INSTALL_DIR)/all.min.js" $(EXTERNS)
	gzip -fk "$(PRODUCTION_INSTALL_DIR)/all.min.js"

buildClientForceDirty:
	stack build --stack-yaml=client.yaml --force-dirty

setupServer:
	stack setup --stack-yaml=server.yaml

buildServer: setupServer
	stack build --stack-yaml=server.yaml

GCC_PREPROCESSOR=stack --stack-yaml=client.yaml exec -- gcc -E -x c -P -C -nostdinc

installClient: buildClient
	cp -Rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin/Estuary.jsexe .
	cp -Rf static/* Estuary.jsexe
	$(GCC_PREPROCESSOR) Estuary.jsexe/index.html.template -o Estuary.jsexe/index.html

prodInstallClient: # make prodBuildClient first!
	rm -rf ./Estuary.jsexe
	cp -Rf $(PRODUCTION_INSTALL_DIR) .
	cp -Rf static/* Estuary.jsexe
	$(GCC_PREPROCESSOR) Estuary.jsexe/index.html.template -DPRODUCTION -o Estuary.jsexe/index.html
	rm -rf Estuary.jsexe/runmain.js
	rm -rf Estuary.jsexe/rts.js
	rm -rf Estuary.jsexe/lib.js
	rm -rf Estuary.jsexe/out.js
	rm -rf Estuary.jsexe/all.js
	rm -rf Estuary.jsexe/out.stats
	rm -rf Estuary.jsexe/index.html.template
	rm -rf Estuary.jsexe/all.min.js.gz

installServer: buildServer
	cp $$(stack path --local-install-root --stack-yaml=server.yaml)/bin/EstuaryServer ./EstuaryServer

prodReleaseClient: # make prodInstallClient first!
	rm -rf temp
	mkdir temp
	cp -Rf Estuary.jsexe temp
	rm -rf temp/Estuary.jsexe/Dirt
	# tar czf estuary-client.tgz -C temp .
	cd temp; zip -r ../estuary-client.zip ./*
	rm -rf temp

curlReleaseClient: # this uses curl to download and unzip a recent pre-built client from a GitHub release
	rm -rf Estuary.jsexe
	curl -o temp.zip -L https://github.com/d0kt0r0/estuary/releases/download/20180917b/estuary-client-20180917b.zip
	unzip temp.zip
	rm -rf temp.zip
	cp -Rf static/Dirt Estuary.jsexe

clean:
	rm -rf Estuary.jsexe
	rm -rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin
	rm -rf $$(stack path --local-install-root --stack-yaml=server.yaml)/bin
	stack clean --stack-yaml=client.yaml
	stack clean --stack-yaml=server.yaml

style:
	cp static/style.css Estuary.jsexe

rggtrn: installClient installServer
	EstuaryServer/EstuaryServer rggtrn

openClient: installClient
	open Estuary.jsexe/index.html
