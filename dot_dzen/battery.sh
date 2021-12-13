#!/bin/sh
#
# original script by lyon8 <lyon8@gmx.net>
# modifications from original by <sean.escriva@gmail.com>
# show your laptop battery state in dzen
# Taken from http://www.webframp.com/2008/08/17/switchdzen/ and modified by Ryan
# Kavanagh <ryanakca@kubuntu.org> to add support for a missing battery and use
# /sys instead of /proc/acpi/battery

BG='#000000'  # dzen backgrounad
FG='#008dd5'  # dzen foreground
W=114         # width of the dzen bar
GW=50         # width of the gauge
GFG='#33ccff' # color of the gauge
GH=7          # height of the gauge
GBG='#333'    # color of gauge background
X=0           # x position
Y=0           # y position
FN='-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-iso8859-1'     # font

LOWBAT=10        # percentage of battery life marked as low
LOWCOL='#ff4747' # color when battery is low
CHGCOL='#60da11' # color when battery is charging
TIME_INT=2       # time interval in seconds to hold display
TIME_ITER=5      # no of iterations in which time is displayed

PREBAR="^i(${HOME}/.dzen/icons/power-bat.xbm) " # caption (also icons are possible)

echo -n $PREBAR #uncomment for an icon

PRESENT=`acpi -b`;
if [ "${PRESENT}" ]; then
    STATUS=`echo ${PRESENT} | grep Charging`;

    RPERC=`echo ${PRESENT} | cut -d, -f2 | sed -e 's/[^0-9]//g'`;

    # draw the bar and pipe everything into dzen
    if [ $RPERC -le $LOWBAT ]; then
	GFG=$LOWCOL;
    fi
    if [ "${STATUS}" ]; then
	GFG=$CHGCOL;
    else
	GFG='#33ccff';
    fi
    echo $RPERC
else
    echo '??'
fi
