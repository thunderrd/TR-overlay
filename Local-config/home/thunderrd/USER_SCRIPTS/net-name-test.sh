#!/bin/sh
echo "Warning: If you have biosdevname installed and you do not"
echo "override /lib/udev/rules/71-biosdevname.rules, this script will"
echo "not give you accurate information."
echo
echo "In this situation, use the following command to get the new name"
echo "for your interface."
echo
echo "biosdevname --policy physical -i <ifname>"
echo
echo "Also, the output could be wrong if you have other udev rules
which"
echo "use the PROGRAM or RUN options to determine the names of network"
echo "devices."
echo
for i in /sys/class/net/*; do
	echo "==$i"
	udevadm test "$i" 2>&1 | grep ^NAME
	echo
done
