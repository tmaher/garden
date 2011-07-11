#!/bin/bash

set -o errexit
umask 0077

cd `dirname $0` && CADIR="$PWD" && cd - > /dev/null
CFGFILE="$CADIR/openssl-create-ca.cnf"
CFG="-config $CFGFILE"

if [ ! -r "$CFGFILE" ]; then
    echo "No config file!  Sad.  Bailing."
    exit 1
fi

echo "This is the script for creating a new CA!"
read -p "Type 'new ca' to confirm: " CONFSTRING
if [ "x$CONFSTRING" != "xnew ca" ]; then
    echo "I knew you weren't serious; bailing"
    echo ""
    exit 1
fi

OUTDIR=`mktemp -d /tmp/scoped-ca-XXXXXX`
cd $OUTDIR

export TIMESTAMP=`TZ=UTC date '+%Y-%m-%dT%H:%M:%SZ'`
export KEYFILE="scoped-$TIMESTAMP.key"
export CERTFILE="scoped-$TIMESTAMP.crt"

openssl req -nodes $CFG -newkey rsa:2048 -x509 -days 1096 \
    -keyout "$KEYFILE" -out "$CERTFILE"

ls -l "$KEYFILE" >/dev/null
ls -l "$CERTFILE" >/dev/null
ls -l $OUTDIR
echo ""
echo "key and cert saved to $OUTDIR"
echo ""
