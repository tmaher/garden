#!/bin/sh

find . -name \*.htm\* -print0 | xargs -0 -n 1 -I ___ perl -i -pne 's,(["\(])imagebucket/,$1http://s3.amazonaws.com/expomuseum-imagebucket/,g; s,http://www.expomuseum.com/imagebucket/,http://s3.amazonaws.com/expomuseum-imagebucket/,g;'  ___
