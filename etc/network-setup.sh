#!/bin/sh

cd $(dirname $0)
sudo cp systemd/resolved.conf /etc/systemd
test -f /etc/wpa_supplicant/wpa_supplicant-wlp3s0.conf || sudo ln -s ~rak/.config/wpa_supplicant /etc/wpa_supplicant/wpa_supplicant-wlp3s0.conf
sudo systemctl enable wpa_supplicant@wlp3s0.service
sudo systemctl enable systemd-networkd
sudo systemctl restart wpa_supplicant@wlp3s0.service
sudo systemctl restart systemd-networkd
echo "If it works, then purge ifupdown"
