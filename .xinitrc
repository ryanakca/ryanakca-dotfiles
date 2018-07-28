#!/bin/sh
if test -f ~/.Xresources; then
    xrdb -merge -I$HOME ~/.Xresources
fi
redshift -m REDSHIFT_MODE  -l 44.22:-76.48 -t 6500:2500 &
${HOME}/bin/icd
# xcompmgr &
trayer --tint black --edge top --SetDockType true --SetPartialStrut true \
  --expand true --width 10 --transparent true --height 12 --align right &
${HOME}/.dzen/battery.sh &
pavucontrol &
urxvtd -q -f -o &
urxvtc &
${HOME}/.screenlayout/SCREENLAYOUT.sh && nitrogen --restore
if [ "PULSE" = "True" ]; then
    sh bin/startaudio &
fi
${HOME}/bin/mice.sh

exec /usr/bin/xmonad
