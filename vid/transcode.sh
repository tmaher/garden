#!/bin/bash

# cribbed from http://muzso.hu/2012/07/11/how-to-encode-h.264-video-with-baseline-profile-and-aac-with-low-complexity-profile-using

if [ "x$1" == "x" ]; then
    echo "Usage: $0 src [dest]"
    exit 0
fi

src="$1"
dest=""
if [ "x$2" == "x" ]; then
    dest=`echo $src | sed -e 's/\.[a-zA-Z0-9]$//'`
    dest="${dest}.mp4"
else
    dest="$2"
fi

echo "src $src dest $dest"

exit 0
