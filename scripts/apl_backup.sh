#!/bin/bash

mst_user="$1"

XORG="/home/$mst_user/.config/xorg.conf"
GETTY="/home/$mst_user/.config/getty@.service"

if [ -f "$XORG" ]; then
    mv $XORG /etc/X11/xorg.conf
fi

if [ -f "$GETTY" ]; then
    mv $GETTY /lib/systemd/system/getty@.service
fi
