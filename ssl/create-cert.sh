#!/bin/bash

set -o errexit
umask 0077

cd `dirname $0` && SSLDIR="$PWD" && cd - >/dev/null
MY_CFGFILE="$SSLDIR/scoped-CA/openssl.cnf"
if [ "x$OPENSSL_CFG_FILE" != "x" ]; then
    MY_CFGFILE="$OPENSSL_CFG_FILE"
fi
CFG="-config $MY_CFGFILE"

if [ ! -d "$HOME/private/ssl/hosts" ]; then
    mkdir -p "$HOME/private/ssl/hosts"
fi

cd "$HOME/private/ssl/hosts"

if [ ! -r $MY_CFGFILE ]; then
  echo "can't find config file $MY_CFGFILE under $SSLDIR"
  exit 1
fi

if  [ x$1 == "x-h" ] || [ x$1 == "x--h" ] || [ x$1 == "x--help" ] || \
    [ $# -eq 0 ]; then
  echo "usage: $0 fqdn [fqdn2 [fqdn3 [fqdn4 ...]]]"
  exit 1
fi

if [ ! -d "$HOME/private/ssl/newcerts" ]; then
    mkdir "$HOME/private/ssl/newcerts"
fi

if [ ! -d "$HOME/private/rand" ]; then
    mkdir "$HOME/private/rand"
fi

if [ ! -r "$HOME/private/ssl/index.txt" ]; then
    touch "$HOME/private/ssl/index.txt"
fi

if [ ! -r "$HOME/private/ssl/serial" ]; then
    echo "00" > "$HOME/private/ssl/serial"
fi


export RECFQDN="$1"

while [ $# -gt 0 ]; do
  _SAN="${_SAN},DNS:$1"
  shift
done
export SUBJECTALTNAME=${_SAN/#,/}

if [ -r ${RECFQDN}.key ]; then
  echo "${RECFQDN}.key exists! aborting"
  exit 1
fi

openssl req ${CFG} -nodes -new -newkey rsa:2048 \
    -keyout ${RECFQDN}.key -out ${RECFQDN}.csr
openssl ca ${CFG} -in ${RECFQDN}.csr -out ${RECFQDN}.crt

ls -l ${RECFQDN}.*
