#!/bin/sh
#
# Updates the internet radio playlist for MPD
# Best run from a cronjob

RUHIT=$(curl -s http://ruhit.fm/player.htm | grep ruhit_64 \
       | sed -e 's/.*="//g;s/".*//g')

cat <<EOF > /var/lib/mpd/playlists/Internet.m3u
#EXTM3U

#EXTINF:-1,CKGN-FM 89.7 (Kapuskasing)
http://stream03.ustream.ca:80/ckgn128.mp3

#EXTINF:-1,CHYK-FM 104.1 (Timmins)
http://rubix.wavestreamer.com:8015/stream/1/

#EXTINF:-1,CJFO-FM 94.5 (Ottawa)
http://stream03.ustream.ca:8000/cjfofm128.mp3

#EXTINF:-1,CINN-FM 99.1 (Hearst)
http://stream2.statsradio.com:8050/stream

#EXTINF:-1,CFSF-FM 99.3 (Sturgeon Falls)
http://listenlive.vistaradio.ca/CFSF

#EXTINF:-1,99.6 Радио Русский Хит
${RUHIT}

#EXTINF:-1,WYEP 91.3 (Pittsburgh)
http://playerservices.streamtheworld.com/api/livestream-redirect/WYEPFMAAC.m3u8
EOF
