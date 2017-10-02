#!/bin/sh
mpc disable "DigiHug USB Audio"
mpc enable "My Pulse Output"
pacmd load-module module-udev-detect
do_dac.sh
