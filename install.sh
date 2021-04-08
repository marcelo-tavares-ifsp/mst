#!/bin/bash

### install.sh -- MST installation helper script

# Copyright (C) 2021 "AZ Company Group" LLC <https://gkaz.ru/>
# Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

build() {
    git submodule init
    git submodule update --remote
    qmake-qt5
    make -j2 build_deps
    make -j2
}

build_ubuntu() {
    git submodule init
    git submodule update --remote
    qmake
    make -j2 build_deps
    make -j2
}

install_mst() {
    make install_deps
    make install
}

install_deps_ubuntu() {
    apt update
    echo ">>> Installing VirtualGL ... "
    ./install_vgl.sh 2.6.3
    echo ">>> Installing VirtualGL ... done"

    echo ">>> Installing required packages ..."
    apt install -y \
        autoconf \
        automake \
        docker.io \
        libudev-dev \
        lightdm \
        awesome \
        unclutter \
        build-essential \
        qt5-default \
        qtbase5-dev \
        guile-2.2 \
        guile-2.2-dev \
        texinfo \
        gettext \
        make

    echo ">>> Installing required packages ... done"
}

install_deps_alt_p8() {
    echo ">>> Installing required packages ..."
    apt-get install -y \
            guile20-devel \
            libguile20-devel \
            texinfo \
            autoconf_2.60 \
            automake_1.14 \
            make \
            gettext \
            awesome \
            docker-ce \
            unclutter \
            lightdm \
            libqt5-core \
            libudev-devel \
            virtualgl \
            Xdialog \
            guile20 \
            libguile20
    echo ">>> Installing required packages ... done"
}

install_deps_alt_p9() {
    echo ">>> Installing required packages ..."
    apt-get install -y \
            guile22-devel \
            texinfo \
            autoconf_2.60 \
            automake_1.14 \
            make \
            gettext \
            awesome \
            docker-ce \
            unclutter \
            lightdm \
            libqt5-core \
            libudev-devel \
            virtualgl \
            Xdialog \
            guile22
    echo ">>> Installing required packages ... done"
}

install_deps_alt() {
    local version=$(lsb_release -r \
			| sed -e 's/Release:.*\([0-9]\).[0-9]/\1/g')
    apt-get update
    if [ "$version" -eq 8 ]; then
	install_deps_alt_p8
    elif [ "$version" -eq 9 ]; then
	install_deps_alt_p9
    else
	echo "ERROR: Unsupported ALT Linux release"
	exit 1
    fi
}

print_help_and_exit() {
cat <<EOF
Usage: ./install.sh <distribution-name>

Supported distributions:
  - alt    -- ALT Linux P8/P9
  - ubuntu -- Ubuntu 20.04

Invocation example:
  ./install.sh alt

EOF

  exit 0
}

# Entry point
main() {
    local distro="$1"
    local version

    case $distro in
        "alt")
            install_deps_alt
            build
            install_mst
            ;;
        "ubuntu")
            install_deps_ubuntu
            build_ubuntu
            install_mst
            ;;
        *)
            print_help_and_exit
            build
            install_mst
            ;;
    esac
    systemctl enable docker
    systemctl start docker
}

main $*
