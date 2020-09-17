#!/bin/bash

SF_URL="https://sourceforge.net/projects/virtualgl/files/"

download_vgl() {
    local version=$1
    local arch=$2
    local extension=$3
    local file_name="virtualgl_${version}_${arch}.${extension}"

    wget --quiet \
         -O "${file_name}" \
         "${SF_URL}/${version}/${file_name}/download"

    echo "${file_name}"
}

install_vgl() {
    local version=$1
    local arch=$2
    local extension=$3
    local file_name="virtualgl_${version}_${arch}.${extension}"

    case $extension in
        "deb")
            # echo "dpkg -i $file_name"
            dpkg -i "$file_name"
            ;;
        "rpm")
            # echo "rpm -i $file_name"
            rpm -i "$file_name"
            ;;
        *)
            echo "ERROR: Unknown package format"
            exit 1
            ;;
    esac
}

main() {
    local version=$1
    local platform=$(uname --hardware-platform)
    local extension
    local arch

    if [ $platform = "x86_64" ]; then
        arch="amd64"
    else
        arch="i386"
    fi

    if [ -z "$(which dpkg)" ]; then
        extension="rpm"
    else
        extension="deb"
    fi

    download_vgl "$version" "$arch" "$extension"
    install_vgl "$version" "$arch" "$extension"
}

main $*
