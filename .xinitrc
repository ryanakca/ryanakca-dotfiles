#!/bin/sh
redshift -m REDSHIFT_MODE  -l 44.22:-76.48 -t 6500:2500 &
/home/ryan/bin/icd
# xcompmgr &
trayer --tint black --edge top --SetDockType true --SetPartialStrut true \
  --expand true --width 10 --transparent true --height 12 --align right &
xautolock -time 2 &
/home/ryan/.dzen/battery.sh &
# /home/ryan/.dzen/temperature.sh &
# /usr/local/libexec/gnome-settings-daemon &
#kmix &
# feh --bg-center /home/ryan/.local/share/wallpapers/skyscraper_at_sunset.bmp
# feh --bg-center /home/ryan/.local/share/wallpapers/totheglory.jpg
urxvtd -q -f -o &
urxvtc &
# /home/ryan/.screenlayout/work.sh
/home/ryan/.screenlayout/nu-home.sh && nitrogen --restore
if [ "True" = "True" ]; then
    sh bin/startaudio &
fi

# Synaptics:
synclient HorizTwoFingerScroll=0 || true
synclient HorizEdgeScroll=1 || true
# Enable circular scrolling with top edge activating
synclient CircularScrolling=1 || true
synclient CircScrollTrigger=1 || true
# One finger is left click
synclient TapButton1=1 || true
# Two is right click
synclient TapButton2=3 || true
# Three is middle click
synclient TapButton3=2 || true
# Enable coasting
synclient CoastingSpeed=5
synclient CoastingFriction=30

exec /usr/bin/xmonad
