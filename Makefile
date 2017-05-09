setup:
	stack setup

build:
	stack build

install:
	cp -Rf $$(stack path --local-install-root)/bin/Estuary.jsexe .
	cp -Rf static/* Estuary.jsexe

open:
	cp -Rf $$(stack path --local-install-root)/bin/Estuary.jsexe .
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe
	open Estuary.jsexe/index.html

clean:
	rm -rf Estuary.jsexe
	rm -rf $$(stack path --local-install-root)/bin/Estuary.jsexe

style:
	cp static/style.css Estuary.jsexe

static:
	cp -Rf static/* Estuary.jsexe

Estuary.jsexe:
	ghcjs -o Estuary Main.hs
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe
