#!/bin/sh

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

trackball=$(xinput | grep "Kensington Expert Wireless TB" | grep pointer | sed -e 's/.*id=//g;s/\s\+.*//g')
if [ "x${trackball}" != "x" ]; then
    xinput set-button-map "${trackball}" 1 2 8 4 5 6 7 3 9 10 11 12 13 14 15 16
    xinput set-prop "${trackball}" "libinput Accel Speed" 0.25
fi


