#!/bin/sh

set -e

systemctl --user start pulseaudio.socket
systemctl --user start pulseaudio.service
mpc disable "DigiHug USB Audio"
mpc enable "My Pulse Output"
pacmd load-module module-udev-detect
${HOME}/bin/do_dac.sh
