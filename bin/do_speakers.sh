#!/bin/sh

USB_CARD="alsa_card.usb-FiiO_DigiHug_USB_Audio-01"
USB_SINK="alsa_output.usb-FiiO_DigiHug_USB_Audio-01.iec958-stereo"
SPEAKERS="alsa_output.pci-0000_00_1b.0.analog-stereo"

pacmd set-sink-mute "${USB_SINK}" 1
pacmd set-sink-mute "${SPEAKERS}" 0

for s in $(pacmd list-sink-inputs | awk '$1 == "index:" {print $2}')
do
    pacmd move-sink-input $s "${SPEAKERS}" >/dev/null 2>&1
done
