#!/bin/sh
redshift -m REDSHIFT_MODE  -l 44.22:-76.48 -t 6500:2500 &
xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/icd $DISPLAY
# xcompmgr &
trayer --tint black --edge top --SetDockType true --SetPartialStrut true \
  --expand true --width 10 --transparent true --height 18 --align right &
xautolock -time 2 &
/home/ryan/.dzen/battery.sh &
/home/ryan/.dzen/temperature.sh &
gnome-settings-daemon &
#kmix &
# feh --bg-center /home/ryan/.local/share/wallpapers/skyscraper_at_sunset.bmp
# feh --bg-center /home/ryan/.local/share/wallpapers/totheglory.jpg
urxvtd -q -f -o &
urxvtc &
# /home/ryan/.screenlayout/work.sh
/home/ryan/.screenlayout/SCREENLAYOUT.sh && nitrogen --restore
exec /usr/local/bin/xmonad
