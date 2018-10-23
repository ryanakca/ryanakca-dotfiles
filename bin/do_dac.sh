#!/bin/sh

USB_CARD="alsa_card.usb-FiiO_DigiHug_USB_Audio-01"
USB_SINK="alsa_output.usb-FiiO_DigiHug_USB_Audio-01.iec958-stereo"
SPEAKERS="alsa_output.pci-0000_00_1b.0.analog-stereo"

pacmd set-sink-mute "${SPEAKERS}" 1
pacmd set-card-profile "${USB_CARD}" output:iec958-stereo
pacmd set-sink-mute "${USB_SINK}" 0

if pacmd list-modules | grep module-ladspa-sink; then
    pacmd unload-module module-ladspa-sink
fi
pacmd load-module module-ladspa-sink sink_name=binaural sink_master="${USB_SINK}" plugin=bs2b label=bs2b control=700,4.5

for s in $(pacmd list-sink-inputs | awk '$1 == "index:" {print $2}')
do
    pacmd move-sink-input $s "${USB_SINK}" >/dev/null 2>&1
done
