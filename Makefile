Estuary.jsexe:
	ghcjs -o Estuary Main.hs
	cp static/index.html Estuary.jsexe
	cp static/style.css Estuary.jsexe

style:
	cp static/style.css Estuary.jsexe
	
static:
	cp -Rf static/* Estuary.jsexe

all: Estuary.jsexe static

clean:
	rm -rf Estuary.jsexe
