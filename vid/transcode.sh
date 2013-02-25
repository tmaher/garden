#!/bin/bash

# cribbed from http://muzso.hu/2012/07/11/how-to-encode-h.264-video-with-baseline-profile-and-aac-with-low-complexity-profile-using

FFMPEG="ffmpeg"

if [ "x$1" == "x" ]; then
    echo "Usage: $0 src [dest]"
    exit 0
fi

src="$1"
dest=""
if [ "x$2" == "x" ]; then
    dest=`echo $src | sed -Ee 's/\.[a-zA-Z0-9]+$//'`
else
    dest="$2"
fi

echo "src $src dest $dest"

if [ `basename -s .mp4 "${dest}"` != "${dest}" ]; then
    dest="${dest}.mp4"
fi

if [ "x$src" == "x$dest" ]; then
    echo "src and dest must be different files!"
    exit 0
fi


if ${FFMPEG} -i "${src}" 2>&1 | grep "Video: h264" >/dev/null; then
    VID_ARGS="-vcodec copy"
else
    VID_ARGS="-vcodec libx264 -profile:v baseline"
fi

${FFMPEG} -i "${src}" -y -f mp4 $VID_ARGS \
    -acodec libfaac -profile:a aac_low -ar 44100 -b:a 128k -ac 2 \
    "${dest}"

exit 0
