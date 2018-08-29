#!/bin/bash

### mk_backup.sh -- Make a backup before MST configuration

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

# Make backup directory.  Return the full directory name.
#
# Syntax:
#   make_backup_dir backup_root_dir
make_backup_dir()
{
    local backup_root_dir="$1"
    local backup_dir="$backup_root_dir/$(date -Iseconds)/"
    if [ ! -d "$backup_dir" ]; then
	mkdir -p "$backup_dir"
    fi

    echo "$backup_dir"
}

# Copy file if it exists.
#
# Syntax:
#   copy_if_exists source destination
copy_if_exists()
{
    local src="$1"
    local dst="$2"
    if [ -f "$src" ]; then
	cp "$src" "$dst"
    fi
}

### Entry point.

main()
{
    local mst_user="$1"
    if [ -z "$mst_user" ]; then
	echo "Usage: $0 mst_user"
	exit 1
    fi

    local backup_root_dir="/var/lib/mst/backup"

    # Configuration files
    local xorg_config="/etc/X11/xorg.conf"
    local getty_config="/lib/systemd/system/getty@.service"
    local awesome_config="/home/$mst_user/.config/awesome/rc.lua"

    local backup_dir=$(make_backup_dir "$backup_root_dir")

    copy_if_exists "$xorg_config" "$backup_dir"
    copy_if_exists "$getty_config" "$backup_dir"
    copy_if_exists "$awesome_config" "$backup_dir"

    exit 0
}

main $*

### mk_backup.sh ends here.
