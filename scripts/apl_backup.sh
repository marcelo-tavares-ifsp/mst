#!/bin/bash

XORG="/home/student/.config/xorg.conf"
GETTY="/home/student/.config/getty@.service"

if [ -f "$XORG" ]; then
    mv $XORG /etc/X11/xorg.conf
fi

if [ -f "$GETTY" ]; then
    mv $GETTY /lib/systemd/system/getty@.service
fi
