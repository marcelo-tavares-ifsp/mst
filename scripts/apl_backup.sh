#!/bin/bash -x

main() {
    local mst_user="$1"
    local last_backup_date="$(ls -1 /var/lib/mst/backup/ | head -1)"
    local xorg_backup="/var/lib/mst/backup/$last_backup_date/xorg.conf"
    local getty_backup="/var/lib/mst/backup/$last_backup_date/getty@.service"

    [ -f "/etc/X11/xorg.conf" ] && rm "/etc/X11/xorg.conf"

    [ -e "/etc/skel" ] && [ -f "/etc/skel/.bashrc" ] \
			      && rm "/etc/skel/.bashrc"

    # if [ -f "$xorg_backup" ]; then
    # 	cp "$xorg_backup" /etc/X11/xorg.conf
    # fi

    [ -e "/etc/systemd/system/systemd-udevd.service" ] \
        && rm "/etc/systemd/system/systemd-udevd.service"

    if [ -f "$getty_backup" ]; then
	cp "$getty_backup" /lib/systemd/system/getty@.service
    fi
}

main $*
