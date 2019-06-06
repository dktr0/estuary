curl -o temp.zip -L https://github.com/dktr0/estuary/releases/download/$1/estuary-client-$1.zip
cd staging 
unzip -o ../temp.zip
cd ..
rm -rf temp.zip
cp -Rf static/samples staging/Estuary.jsexe
