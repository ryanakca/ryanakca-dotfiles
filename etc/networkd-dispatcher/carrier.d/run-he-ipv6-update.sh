#!/bin/sh

if test $IFACE = "he-ipv6" -a $AdministrativeState = "configured"; then
	systemctl restart he-ipv6-update.service
fi
