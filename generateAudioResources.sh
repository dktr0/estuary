#/bin/sh
printf "[\n"
dircount=0
find $1 -mindepth 1 -maxdepth 1 -iname "*" | sort | while read d; do
  if [ -d "$d" ]
  then
    dirname=`basename $d`
    # printf "\"%s\": [" "$dirname"
    search2=$searchRoot/$dirname/*.WAV
    filecount=0
    find "$d" -iname "*.wav" | sort | while read f; do
    # for f in $search2; do
      filename=$(printf %q "$f")
      basename=${f##*/}
      if [[ ($dircount -ne 0) || ($filecount -ne 0) ]]; then
          printf ",\n"
      fi
      if [[ ${basename:0:1} != "." ]]; then
        printf "{ \"url\":"
        printf "\"%s/%s\"" "$dirname" "$basename"
        printf ",\"type\":\"audio\","
        printf "\"bank\":\"%s\"," "$dirname"
        printf "\"n\":%d" $filecount
        printf "}"
        (( filecount++ ))
      fi
    done
    (( dircount++ ))
  fi
done
printf "\n]\n"
