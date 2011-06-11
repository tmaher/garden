#!/bin/sh

CFGFILE="../CA/openssl.cnf"
CFG="-config $CFGFILE"

SSLDIR=`dirname $0`
cd ${SSLDIR}/hosts

if [ ! -r $CFGFILE ]; then
  echo "can't find config file $CFGFILE under $SSLDIR"
  exit 1
fi

if [ x$1 == "x" ]; then
  echo "usage: $0 fqdn"
  exit 1
fi

fqdn=$1
if [ -r ${fqdn}* ]; then
  echo "${fqdn}* exists! aborting"
  exit 1
fi

openssl req ${CFG} -nodes -new -newkey rsa:2048 -keyout ${fqdn}.key -out ${fqdn}.csr
openssl ca ${CFG} -in ${fqdn}.csr -out ${fqdn}.crt

ls -l ${fqdn}.*
