rm -rf Estuary.jsexe
curl -o temp.zip -L https://github.com/dktr0/estuary/releases/download/$1/estuary-client-$1.zip
unzip temp.zip
rm -rf temp.zip
cp -Rf static/samples Estuary.jsexe
