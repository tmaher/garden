#!/bin/bash -x

export _appdir=$(cd $(dirname "${0}") && pwd) || exit 1
cd ${_appdir} || exit 1

mkdir -p html

# BASE="https://kursbuch.bahn.de/hafas/kbview.exe/dn?searchmode=station&mainframe=result&orig=sT&dosearch=1&line_nr="
BASE="https://kursbuch.bahn.de/hafas/kbview.exe/dn?searchmode=tableplus&mainframe=result&dosearch=1&table_nr="

let i=500
let i_max=700
let interval=0
while [ ${i} -lt ${i_max} ]; do
  curl -s --retry 5 --retry-delay 3 -o "html/${i}.html" "${BASE}${i}" &
  let i++
  echo "$i done"
  let interval++
  if [ ${interval} -gt 14 ]; then
    echo "sleeping" && sleep 2
    let interval=0
    wait
  fi
done

cat html/*.html | grep '\.pdf\?' | sort -u | sed -e 's/\"//g' > html/list

echo "PDF urls in html/list"
exit 0
