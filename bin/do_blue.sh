#!/bin/sh
pacmd load-module module-bluez5-discover
echo "connect CD:0D:69:69:9A:1B" | bluetoothctl
