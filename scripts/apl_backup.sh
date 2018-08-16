#!/bin/bash

main() {
    local mst_user="$1"
    local last_backup_date="$(ls -1 /var/lib/mst/backup/ | head -1)"
    local xorg_backup="/var/lib/$last_backup_date/xorg.conf"
    local getty_backup="/var/lib/$last_backup_date/getty@.service"

    if [ -f "$xorg_backup" ]; then
	cp "$xorg_backup" /etc/X11/xorg.conf
    fi

    if [ -f "$getty_backup" ]; then
	cp "$getty_backup" /lib/systemd/system/getty@.service
    fi
}

main $*
