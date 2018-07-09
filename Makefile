setupClient:
	stack setup --stack-yaml=client.yaml

buildClient: setupClient
	stack build --stack-yaml=client.yaml

buildClientForceDirty:
	stack build --stack-yaml=client.yaml --force-dirty

setupServer:
	stack setup --stack-yaml=server.yaml

buildServer: setupServer
	stack build --stack-yaml=server.yaml

installClient: buildClient
	cp -Rf $$(stack path --local-install-root --stack-yaml=client.yaml)/bin/Estuary.jsexe .
	cp -Rf static/* Estuary.jsexe

installServer: buildServer
	cp $$(stack path --local-install-root --stack-yaml=server.yaml)/bin/EstuaryServer ./EstuaryServer

rggtrn: installClient installServer
	EstuaryServer/EstuaryServer rggtrn 8002

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
