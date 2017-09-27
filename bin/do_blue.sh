#!/bin/sh
pacmd load-module module-bluez5-discover
echo "connect 00:01:01:00:00:12" | bluetoothctl
