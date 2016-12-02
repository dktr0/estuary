Estuary.jsexe:
	ghcjs Main.hs -o Estuary
	cp index.html Estuary.jsexe/index.html
all: Estuary.jsexe
