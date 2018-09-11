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

prodInstallClient: prodBuildClient
	cp -Rf $(PRODUCTION_INSTALL_DIR) .
	cp -Rf static/* Estuary.jsexe
	$(GCC_PREPROCESSOR) Estuary.jsexe/index.html.template -DPRODUCTION -o Estuary.jsexe/index.html

installServer: buildServer
	cp $$(stack path --local-install-root --stack-yaml=server.yaml)/bin/EstuaryServer ./EstuaryServer

rggtrn: installClient installServer
	EstuaryServer/EstuaryServer rggtrn

openClient:
	cp -Rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin/Estuary.jsexe .
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe
	open Estuary.jsexe/index.html

zipClient:
	rm -rf temp
	mkdir temp
	cp package.json temp
	cp estuary.js temp
	cp evalClient.js temp
	cp -Rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin/Estuary.jsexe temp
	cp static/index.html temp/Estuary.jsexe
	cp static/style.css temp/Estuary.jsexe
	cp static/EstuaryProtocol.js temp/Estuary.jsexe
	tar czf estuary-build.tgz -C temp .
	rm -rf temp

zipClientWithWebDirt:
	rm -rf temp
	mkdir temp
	cp static/WebDirt/sampleMap.json temp
	cp evalClient.js temp
	cp -Rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin/Estuary.jsexe temp
	cp static/index.html temp/Estuary.jsexe
	cp static/style.css temp/Estuary.jsexe
	cp static/EstuaryProtocol.js temp/Estuary.jsexe
	cp -Rf static/WebDirt temp/Estuary.jsexe
	cd temp; zip -r ../estuary-build.zip ./*
	rm -rf temp

WebDirt:
	cp -Rf static/WebDirt Estuary.jsexe

clean:
	rm -rf Estuary.jsexe
	rm -rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin
	rm -rf $$(stack path --local-install-root --stack-yaml=server.yaml)/bin
	stack clean --stack-yaml=client.yaml
	stack clean --stack-yaml=server.yaml

style:
	cp static/style.css Estuary.jsexe

static:
	cp -Rf static/* Estuary.jsexe

Estuary.jsexe:
	ghcjs -o Estuary Main.hs
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe
