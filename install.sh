#!/bin/bash

### install.sh -- MST installation helper script

# Copyright (C) 2021-2022 "AZ Company Group" LLC <https://gkaz.ru/>
# Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

STEP_MARKER=">>>"

log_step() {
    local message="$*"
    echo -e "\e[32m${STEP_MARKER}\e[0m $message"
}

log_error() {
    local message="$*"
    echo -e "\e[31m${STEP_MARKER} ERROR: $message \e[0m"
}

log_info() {
    local message="$*"
    echo -e "░░░ $message"
}

build() {
    git submodule init
    git submodule update --remote
    qmake-qt5
    make -j$(nproc) build_deps
    make -j$(nproc)
}

build_ubuntu() {
    git submodule init
    git submodule update --remote
    qmake
    make -j$(nproc) build_deps
    make -j$(nproc)
}

install_mst() {
    make -j$(nproc) install_deps
    make -j$(nproc) install
}

install_deps_ubuntu() {
    apt update
    log_step "Installing VirtualGL ... "
    ./install_vgl.sh 2.6.3
    log_step "Installing VirtualGL ... done"

    log_step "Installing required packages ..."
    apt install -y \
        autoconf \
        automake \
        docker.io \
        libudev-dev \
        libxfixes-dev \
        lightdm \
	lightdm-gtk-greeter \
        awesome \
        build-essential \
        qt5-default \
        qtbase5-dev \
	qttools5-dev-tools \
        guile-2.2 \
        guile-2.2-dev \
        texinfo \
        gettext \
        make

    log_step "Installing required packages ... done"
}

install_deps_ubuntu_22_04() {
    apt update
    log_step "Installing VirtualGL ... "
    ./install_vgl.sh 2.6.3
    log_step "Installing VirtualGL ... done"

    log_step "Installing required packages ..."
    apt install -y \
        autoconf \
        automake \
        docker.io \
        libudev-dev \
        libxfixes-dev \
        lightdm \
	lightdm-gtk-greeter \
        awesome \
        build-essential \
        qtbase5-dev \
        qttools5-dev \
        guile-2.2 \
        guile-2.2-dev \
        xserver-xorg \
        texinfo \
        gettext \
        make

    log_step "Installing required packages ... done"
}

install_deps_alt_p8() {
    log_step "Installing required packages ..."
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
            lightdm \
            libqt5-core \
            libudev-devel \
            virtualgl \
            Xdialog \
            guile20 \
            libguile20
    log_step "Installing required packages ... done"
}

install_deps_alt_p9() {
    log_step "Installing required packages ..."
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
            lightdm \
            libqt5-core \
            libtool \
            qt5-base-devel \
            qt5-tools \
            libudev-devel \
            virtualgl \
            Xdialog \
            guile22
    log_step "Installing required packages ... done"
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
        log_error "Unsupported ALT Linux release"
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
                log_error "Unsupported ALT Linux release:" $version
                exit 1
            fi
            log_info "MST for ALT Linux P${version} is going to be installed."
            read -p "Continue? (y/n) "
            if [ ! "$REPLY" == "y" ] && [ ! "$REPLY" == "Y" ]; then
                echo "Exiting..."
                exit 0
            fi
            install_deps_alt $version
            build
            install_mst
            ;;
        "ubuntu")
            version=$(alt_get_version)
            log_info "MST for Ubuntu ${version} is going to be installed."
            read -p "Continue? (y/n) "
            if [ ! "$REPLY" == "y" ] && [ ! "$REPLY" == "Y" ]; then
                echo "Exiting..."
                exit 0
            fi

            case $version in
                22)
                    install_deps_ubuntu_22_04
                   ;; 
                *)
                    install_deps_ubuntu
                    ;;
            esac
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
