#!/bin/sh
#
# Updates the internet radio playlists for MPD
# Best run from a cronjob

BASE=/var/lib/mpd/playlists/

playlist() {
    cat<<EOF > "${BASE}/radio-$1.m3u"
#EXTM3U
#EXTINF:-1,$2
$3
EOF
}

getstream() {
    curl -s "$1" | grep '^File1=' | sed -e 's/File1=//g'
}

playlist "1920s"   "1920s Radio Network"           "$(getstream 'http://kara.fast-serv.com:8398/listen.pls')"
# playlist "CFSF-FM" "CFSF-FM 99.3 (Sturgeon Falls)" 'http://listenlive.vistaradio.ca/CFSF'
# playlist "CHYK-FM" "CHYK-FM 104.1 (Timmins)"       'http://rubix.wavestreamer.com:8015/stream/1/'
playlist "CINN-FM" "CINN-FM 99.1 (Hearst)"         'http://stream2.statsradio.com:8050/stream'
# playlist "CJFO-FM" "CJFO-FM 94.5 (Ottawa)"         'http://stream03.ustream.ca:8000/cjfofm128.mp3'
playlist "CKGN-FM" "CKGN-FM 89.7 (Kapuskasing)"    'http://stream03.ustream.ca:80/ckgn128.mp3'
playlist "WYEP"    "WYEP 91.3 (Pittsburgh)"        'https://ais-sa3.cdnstream1.com/2557_128.mp3'
playlist "WZUM"    "WZUM 88.1 (Pittsburgh)"        'http://pubmusic.streamguys1.com/wzum-aac'
playlist "Dismuke" "Radio Dismuke"                 "$(getstream 'https://early1900s.org/radiodismuke/radiodismuke.pls')"
playlist "russhit" "99.6 Радио Русский Хит"        "$(curl -s http://ruhit.fm/player.htm | grep ruhit_64 | sed -e 's/.*="//g;s/".*//g')"

# http://colombiacrossover.com/
playlist "salsa.dura" "Colombia Salsa Dura"        "$(getstream 'http://64.37.50.226:8054/listen.pls?sid=1')"

# http://www.tenientiko.com/
playlist "salsa.catedral" "La Catedral de la Salsa" "$(getstream 'http://176.31.120.166:4450/listen.pls?sid=1')"

# http://www.rockolapegassera.com
playlist "cumbia.rockola" "Rockola Pegassera 107.9" "$(getstream 'http://54.39.19.215:8004/listen.pls?sid=1')"

playlist "salsa.metro" "El Metro Salsero" 'http://s5.voscast.com:7516/stream'
