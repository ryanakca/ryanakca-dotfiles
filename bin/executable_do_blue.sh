#!/bin/sh
pacmd list-modules module-bluez5-discover | grep -q module-bluez5-discover || \
    pacmd load-module module-bluez5-discover
echo "connect CD:0D:69:69:9A:1B" | bluetoothctl
