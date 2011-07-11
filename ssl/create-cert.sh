#!/bin/bash

set -o errexit
umask 0077

cd `dirname $0` && SSLDIR="$PWD" && cd - >/dev/null
CFGFILE="$SSLDIR/scoped-CA/openssl.cnf"
CFG="-config $CFGFILE"

cd "$HOME/private/ssl/hosts"

if [ ! -r $CFGFILE ]; then
  echo "can't find config file $CFGFILE under $SSLDIR"
  exit 1
fi

if  [ x$1 == "x-h" ] || [ x$1 == "x--h" ] || [ x$1 == "x--help" ] || \
    [ $# -eq 0 ]; then
  echo "usage: $0 fqdn [fqdn2 [fqdn3 [fqdn4 ...]]]"
  exit 1
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
