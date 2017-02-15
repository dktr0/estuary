Estuary.jsexe:
	ghcjs -o Estuary Main.hs
	cp index.html Estuary.jsexe

static: 
	cp -Rf static/* Estuary.jsexe

all: Estuary.jsexe static

clean:
	rm -rf Estuary.jsexe
