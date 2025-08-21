#!/bin/sh

set -e

systemctl --user stop pulseaudio.socket
mpc enable "DigiHug USB Audio"
mpc disable "My Pulse Output"
