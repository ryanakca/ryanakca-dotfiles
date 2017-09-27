#!/bin/sh
pacmd set-card-profile alsa_card.usb-FiiO_DigiHug_USB_Audio-01 output:iec958-stereo
pacmd load-module module-ladspa-sink sink_name=binaural sink_master=alsa_output.usb-FiiO_DigiHug_USB_Audio-01.iec958-stereo plugin=bs2b label=bs2b control=700,4.5
