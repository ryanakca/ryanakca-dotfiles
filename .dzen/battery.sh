#!/bin/sh
#
# original script by lyon8 <lyon8@gmx.net>
# modifications from original by <sean.escriva@gmail.com>
# show your laptop battery state in dzen
# Taken from http://www.webframp.com/2008/08/17/switchdzen/ and modified by Ryan
# Kavanagh <ryanakca@kubuntu.org> to add support for a missing battery and use
# /sys instead of /proc/acpi/battery
 
BG='#171717'  # dzen backgrounad
FG='#008dd5'  # dzen foreground
W=114         # width of the dzen bar
GW=50         # width of the gauge
GFG='#33ccff' # color of the gauge
GH=7          # height of the gauge
GBG='#333'    # color of gauge background
X=0           # x position
Y=0           # y position
FN='snap'     # font
 
STATEFILE='/sys/bus/acpi/drivers/battery/PNP0C0A:00/power_supply/BAT1/uevent' # battery's state file
 
LOWBAT=10        # percentage of battery life marked as low
LOWCOL='#ff4747' # color when battery is low
CHGCOL='#60da11' # color when battery is charging
TIME_INT=30      # time intervall in seconds
 
PREBAR='^i(/home/ryan/.dzen/icons/power-bat.xbm) ' # caption (also icons are possible)
 
while true; do
    PRESENT=`acpi -b`;
    if [ "${PRESENT}" ]; then
        BAT_FULL=`cat $STATEFILE|grep POWER_SUPPLY_CHARGE_FULL_DESIGN | cut -d '=' -f 2 `;
        STATUS=`echo ${PRESENT} | grep Charging`;

#        echo $BAT_FULL " " $STATUS " " $RCAP > /dev/stdout

        RPERC=`echo ${PRESENT} | cut -d, -f2 | sed -e 's/[^0-9]//g'`;
        echo ${RPERC} > /tmp/rperc
         
        # draw the bar and pipe everything into dzen
        if [ $RPERC -le $LOWBAT ]; then
            GFG=$LOWCOL;
        fi
        if [ "${STATUS}" ]; then
            GFG=$CHGCOL;
        else
            GFG='#33ccff';
        fi
        echo -n $PREBAR #uncomment for an icon
        eval echo $RPERC | dzen2-gdbar -h $GH -w $GW -fg $GFG -bg $GBG
    else
        echo -n $PREBAR #uncomment for an icon
        eval echo 'Missing'
    fi
    sleep $TIME_INT;
done | dzen2 -ta c -tw $W -y $Y -x $X -fg $FG -bg $BG #-fn $FN
