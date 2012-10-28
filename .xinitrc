#!/bin/sh
redshift -m REDSHIFT_MODE  -l 52.21:0.13 -t 6500:2500 &
/home/ryan/bin/icd
# xcompmgr &
trayer --tint black --edge top --SetDockType true --SetPartialStrut true \
  --expand true --width 10 --transparent true --height 12 --align right &
xautolock -time 2 &
/home/ryan/.dzen/battery.sh &
# /home/ryan/.dzen/temperature.sh &
# GSD_PATH &
#kmix &
# feh --bg-center /home/ryan/.local/share/wallpapers/skyscraper_at_sunset.bmp
# feh --bg-center /home/ryan/.local/share/wallpapers/totheglory.jpg
urxvtd -q -f -o &
urxvtc &
# /home/ryan/.screenlayout/work.sh
/home/ryan/.screenlayout/SCREENLAYOUT.sh && nitrogen --restore
if [ "PULSE_JACKD" = "True" ]; then
    sh bin/startaudio &
fi
exec /usr/bin/xmonad
