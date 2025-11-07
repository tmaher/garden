#!/bin/bash -x

export _appdir=$(cd $(dirname "${0}") && pwd) || exit 1
cd ${_appdir} || exit 1

if [ ! -r html/list ]; then
  echo "error: run fetch-html.sh first"
  exit 1
fi

mkdir -p pdfs && cd pdfs
let interval=0
for url in $(cat ../html/list); do
  echo "fetching ${url}"
  file=$(echo "${url}" | cut -f 7 -d / | cut -f 1 -d \?)
  if [ -r "${file}" ]; then
    echo "CACHE HIT - ${file} - skipping"
    continue
  fi
  curl -s --retry 5 --retry-delay 3 -O "${url}" &
  let interval++
  if [ ${interval} -gt 14 ] ; then
    wait
    echo sleep && sleep 5
    let interval=0
  fi
done

