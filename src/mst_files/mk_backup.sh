#!/bin/bash

CONFDIR="/home/student/.config/mst1.0"
XORG="/etc/X11/xorg.conf"
GETTY="/lib/systemd/system/getty@.service"

if [ ! -d "$CONFDIR" ]; then
    mkdir $CONFDIR
fi

if [ -f "$XORG" ]; then
    cp $XORG /home/student/.config/mst1.0/xorg.conf
fi

if [ -f "$GETTY" ]; then
    cp $GETTY /home/student/.config/mst1.0/getty@.service
fi
