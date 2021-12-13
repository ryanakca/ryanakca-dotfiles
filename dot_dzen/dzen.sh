#!/bin/sh
 
typeset -A DISKS
###
# Config
###
DATE_FORMAT="%a %d %b, %Y"
TIME_ZONES=("America/Toronto" "UTC")
DISKS=(home /home root /)
SEPERATOR=' ^fg(#86AA3F)^c(3)^fg() '
BAR_BG='#7DA926'
BAR_FG='#B9D56E'
BAR_HH=6
BAR_HW=40
BAR_VH=12
BAR_VW=3
BAR_ARGS="-bg $BAR_BG -fg $BAR_FG -w $BAR_HW -h $BAR_HH"
ICON_DIR="$HOME/.dzen/icons/"
NETWORK_INTERFACE=eth0
NET_DOWN_MAX=55
NET_UP_MAX=14
MAILDIR=~/.maildb/GMAIL/
 
GLOBALIVAL=1m
DATEIVAL=60
TIMEIVAL=1
DISKIVAL=1
#CPUTEMPIVAL=5
#CPUIVAL=1
#NPIVAL=3
NETIVAL=1
 
 
###
# Functions
###
_date()
{
    date +${DATE_FORMAT}
}
 
_time()
{
    local zone
    print_space=0
    for zone in $TIME_ZONES; do
        [[ $print_space -eq 1 ]] && print -n " "
        print -n "${zone:t}: $(TZ=$zone date '+%H:%M')"
        print_space=1
    done
}
 
#
# Format: label1 mountpoint1 label2 mountpoint2 ... labelN mountpointN
# Copied and modified from Rob
get_disk_usage() {
    local rstr; local tstr; local i; local sep
    for i in ${(k)DISKS}; do
        tstr=$(print `df -h $DISKS[$i]|sed -ne 's/^.* \([0-9]*\)% .*/\1/p'` 100 | \
            dzen2-gdbar -h $BAR_HH -w $BAR_HW -fg $BAR_FG -bg $BAR_BG -l "${i}" -nonl | \
            sed 's/[0-9]\+%//g;s/  / /g')
        if [ ! -z "$rstr" ]; then
            sep=${SEPERATOR}
        fi
        rstr="${rstr}${sep}${tstr}"
    done
    print -n $rstr
}
 
# Requires mesure
get_net_rates() {
    local up; local down
    up=`mesure -K -l -c 3 -t -o $NETWORK_INTERFACE`
    down=`mesure -K -l -c 3 -t -i $NETWORK_INTERFACE`
    echo "$down $up"
}
 
#cpu_temp()
#{
#    print -n ${(@)$(</proc/acpi/thermal_zone/THRM/temperature)[2,3]}
#}
#
#np()
#{
#    #MAXPOS="100"
#    CAPTION="^i(${ICON_DIR}/musicS.xbm)"
#    #POS=`mpc | sed -ne 's/^.*(\([0-9]*\)%).*$/\1/p'`
#    #POSM="$POS $MAXPOS"
#    print -n "$CAPTION "
#    mpc | head -n1 | tr -d '\n'
#    #echo "$POSM" | gdbar -h 7 -w 50 -fg $BAR_FG -bg $BAR_BG
#}
#
#cpu()
#{
#    gcpubar -c 2 -bg $BAR_BG -fg $BAR_FG -w $BAR_HW -h $BAR_HH | tail -n1 | tr -d '\n'
#}
 
has_new_mail() {
    find ${MAILDIR}/*/new -not -type d | wc -l
}
 
DATEI=0
TIMEI=0
DISKI=0
#NPI=0
#CPUTEMPI=0
#CPUI=0
NETI=0
 
date=$(_date)
times=$(_time)
disk_usage=$(get_disk_usage)
#now_playing=$(np)
#temp=$(cpu_temp)
#cpumeter=$(cpu)
net_rates=( `get_net_rates` )
 
while true; do
    [[ $DATEI -ge $DATEIVAL ]] && date=$(_date) && DATEI=0
    [[ $TIMEI -ge $TIMEIVAL ]] && times=$(_time) && TIMEI=0
    [[ $DISKI -ge $DISKIVAL ]] && disk_usage=$(get_disk_usage) && DISKI=0
    #[[ $NPI -ge $NPIVAL ]] && now_playing=$(np) && NPI=0
    #[[ $CPUI -ge $CPUIVAL ]] && cpumeter=$(cpu) && CPUI=0
    #[[ $CPUTEMPI -ge $CPUTEMPIVAL ]] && temp=$(cpu_temp) && CPUTEMPI=0
    [[ $NETI -ge $NETIVAL ]] && net_rates=( `get_net_rates` ) && NETI=0
 
    # Disk usage
    echo -n "${disk_usage}${SEPERATOR}"
    # Network
    echo $net_rates[1] | dzen2-gdbar -nonl -s v -w $BAR_VW -h $BAR_VH -min 0 \
        -max $NET_DOWN_MAX -fg $BAR_FG -bg $BAR_BG
    echo -n " "
    echo $net_rates[2] | dzen2-gdbar -nonl -s v -w $BAR_VW -h $BAR_VH -min 0 \
        -max $NET_UP_MAX -fg $BAR_FG -bg $BAR_BG
    echo -n "${SEPERATOR}"
    # Mail notification
    if [ `has_new_mail` -gt 0 ]; then
        echo -n "^fg(#73d216)"
    fi
    echo -n "^i(${ICON_DIR}/mail.xbm)^fg()${SEPERATOR}"
    # Time and date
    echo -n "${times}${SEPERATOR}"
    echo -n "${date}"
    echo
 
    DATEI=$(($DATEI+1))
    TIMEI=$(($TIMEI+1))
    DISKI=$(($DISKI+1))
    #NPI=$(($NPI+1))
    #CPUI=$(($CPUI+1))
    #CPUTEMPI=$(($CPUTEMPI+1))
    NETI=$(($NETI+1))
 
    sleep $GLOBALIVAL
done
