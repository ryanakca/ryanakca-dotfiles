#!/bin/sh
#
# original script by lyon8 <lyon8@gmx.net>
# modifications from original by <sean.escriva@gmail.com>
# show your laptop battery state in dzen
# Taken from http://www.webframp.com/2008/08/17/switchdzen/ and modified by Ryan
# Kavanagh <ryanakca@kubuntu.org> to add support for a missing battery and use
# /sys instead of /proc/acpi/battery
 
BG='#171717'  # dzen backgrounad
FG='#11DD11'  # dzen foreground
W=130         # width of the dzen bar
GW=50         # width of the gauge
GFG='#33ccff' # color of the gauge
GH=7          # height of the gauge
GBG='#333'    # color of gauge background
X=922         # x position
Y=0           # y position
FN='snap'     # font
TIME_INT=10
 
TEMPERATURE=`acpi -t`
 
while true; do
    echo $TEMPERATURE | sed -e 's/.*ok, //g' && sleep $TIME_INT
done | dzen2 -ta c -tw $W -y $Y -x $X -fg $FG -bg $BG #-fn $FN
