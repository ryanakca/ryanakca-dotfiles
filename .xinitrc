#!/bin/sh

case `uname` in
    Darwin)
        exec icewm-session
        ;;
    *)
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
        ${HOME}/.screenlayout/zeta-home.sh
        if [ "false" = "True" ]; then
            sh bin/startaudio &
        fi
        ${HOME}/bin/mice.sh
        xset dpms 180 300 600

        exec /usr/bin/xmonad
        ;;
esac
