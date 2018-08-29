#!/bin/bash

### apl_backup.sh -- Restore files from a backup.

# Copyright (C) 2018 Anton Plekhanov <plehunov.anton9@gmail.com>
# Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
#
# This file is part of MST.
#
# MST is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# MST is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MST.  If not, see <http://www.gnu.org/licenses/>.

###

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

### apl_backup.sh ends here.
