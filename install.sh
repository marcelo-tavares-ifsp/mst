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

MST_USER="multiseat"

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

add_multiseat_user() {
    useradd -m -s /bin/bash $MST_USER
}

add_multiseat_user_to_wheel_group() {
    usermod -a -G wheel $MST_USER
}

ubuntu_add_user_to_groups() {
    usermod -a -G docker $MST_USER
    usermod -a -G video $MST_USER
    usermod -a -G render $MST_USER
    usermod -a -G sudo $MST_USER
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
            automake \
            make \
            gettext \
            gettext-tools \
            awesome \
            docker-ce \
            unclutter \
            lightdm \
            libqt5-core \
	    libtool \
            qt5-base-devel \
	    qt5-tools \
            libudev-devel \
            virtualgl \
            Xdialog \
            guile22
    echo ">>> Installing required packages ... done"
}

alt_get_version() {
    local version
    if $(command -V lsb_release &> /dev/null); then
	version=$(lsb_release -r \
		      | sed -e 's/Release:.\([0-9]\+\).[0-9]\+.*$/\1/g')
    else
	version=$(cat /etc/system-release \
		      | sed -e 's/.* \([0-9]\).[0-9].*/\1/g')
    fi
    echo $version
}

alt_version_supported_p() {
    local version=$1
    case $version in
	8 | 9 | 10)
	    echo true
	    ;;
	*)
	    echo false
	    ;;
    esac
}

install_deps_alt() {
    local version="$1"
    if [ "$version" -eq 8 ]; then
	apt-get update
	install_deps_alt_p8
    elif [ "$version" -eq 9 ] || [ "$version" -eq 10 ]; then
	apt-get update
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
  - alt    -- ALT Linux P8/P9/P10
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
	    version=$(alt_get_version)
	    if [ $(alt_version_supported_p $version) == false ]; then
		echo "ERROR: Unsupported ALT Linux release:" $version
		exit 1
	    fi
	    echo "MST for ALT Linux P${version} is going to be installed."
	    read -p "Continue? (y/n) "
	    if [ ! "$REPLY" == "y" ] && [ ! "$REPLY" == "Y" ]; then
		echo "Exiting..."
		exit 0
	    fi
	    add_multiseat_user
	    install_deps_alt $version
	    add_multiseat_user_to_wheel_group
	    build
	    install_mst
	    ;;
        "ubuntu")
	    add_multiseat_user
            install_deps_ubuntu
            ubuntu_add_user_to_groups
            build_ubuntu
            install_mst
            ;;
        *)
            print_help_and_exit
            ;;
    esac
    systemctl enable docker
    systemctl start docker
}

main $*
