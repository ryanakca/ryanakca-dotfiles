#!/bin/sh
if test -f ~/.Xresources; then
    xrdb -merge -I$HOME ~/.Xresources
fi
redshift &
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
slack &
xset dpms 180 300 600
pgrep -u $(whoami) parcimonie >/dev/null || parcimonie >/dev/null &

exec /usr/bin/xmonad
