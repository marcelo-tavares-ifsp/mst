![MST Logo](doc/logo.png)

# MST

MST (Multi-Seat Toolkit) is a graphical [multi-seat configurator](https://en.wikipedia.org/wiki/Multiseat_configuration)
and a set of tools that enables easy configuration of multi-seat setups.

[![pipeline status](https://gitlab.com/gkaz/mst/badges/master/pipeline.svg)](https://gitlab.com/gkaz/mst/-/commits/master)

## Features
- Graphical configurator that allows to bind mice, keyboards and USB
  sockets (to connect USB drives) to seats.
- Several outputs of one GPU can be used to configure separate seats.
- `mstd` daemon auto-mounts USB drives by means of UDev to specific
  seats and users.

## License
The project was initially written and being developed by "AZ Company
Group" LLC (https://www.gkaz.ru/)  You can find the list of
contributors in `AUTHORS` file.

MST is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.  Please see `COPYING` file for the terms of
GNU General Public License.

The logo (`doc/logo.svg` and rasterised versions) is distributed under
terms of [Creative Commons Attribution-ShareAlike 4.0
International](https://creativecommons.org/licenses/by-sa/4.0/).

## Dependencies

### Runtime

* Awesome
* Docker
* lightdm
* bash
* libqt5-core
* libudev-devel
* virtualgl
* Xdialog (needed for mst-umount)
* guile20
* libguile20

### Build-time
* Automake
* Autoconf
* make
* guile20-devel
* libguile20-devel
* texinfo
* gettext

## Building and installation
```
$ git clone https://gitlab.com/gkaz/mst.git
$ cd mst
$ git submodule init
$ git submodule update --remote
$ qmake-qt5
$ make build_deps
$ make
$ sudo make install_deps
$ sudo make install
```

### Building an RPM package on ALT Linux

An RPM package for the project can be built as follows:
```
$ make -j4 rpm
```
(You should specify apropriate `-j` option according to the number 
of available CPU cores to speed up the building process.)

Installation from the RPM package:

```
$ apt-get install \
    awesome \
    libqt5-core \
    libudev-devel \
    virtualgl \
    qt5-base-devel \
    rpmdevtools
$ useradd multiseat
$ rpm -i mst-1.0.0-1.x86_64.rpm
```

## Ubuntu GNU/Linux (experimental)
Install the needed components:
```
$ sudo apt-get install \
    autoconf \
    automake \
    make \
    docker.io \
    libudev-dev \
    awesome \
    build-essential \
    qt5-default \
    qtbase5-dev \
    guile-2.2 \
    guile-2.2-dev \
    texinfo \
    gettext
```

Build and install MST:
```
$ qmake
$ make build_deps
$ make
$ make install_deps
$ make install
```

Ubuntu GNU/Linux doesn't have VirtualGL in the official repository, so
you should download `.deb` packages for your system manually from here:
https://sourceforge.net/projects/virtualgl/files/2.6.3/

There's a convenient script that allows to download and install
VirtualGL automatically for your distribution:
```
$ sudo ./install_vgl.sh 2.6.3
```

## Usage

To make a multi-seat configuation, you should run `mst` program as the
superuser (`root`) and go through the dialogs.

MST assigns a choosen USB port for each seat. When a USB stick is plugged to a
assigned USB port, it should mount automatically by means of udisks2.

To unmount an USB for the current user you should run `mst-umount`
command without any arguments.  The user should be allowed to run
`umount` command without a password.

Seat configuration is stored in `/etc/mst-seats` file.

### The "mstd" daemon

The daemon is managed by SystemD.  For example, you can restart the
daemon by the following command:

```
$ sudo systemctl restart mstd
```

Note that when daemon is stopped, the multiseat configuration is
stopped too.

## Disabling and enabling mst configuration
To disable mst manually, you must issue the following commands:
```
$ sudo systemctl stop mstd
$ sudo systemctl disable mstd
$ sudo systemctl set-default graphical
```

You can restore the multiseat configuration as follows:
```
$ sudo systemctl enable mstd
$ sudo systemctl start mstd
$ sudo systemctl set-default multi-user
```
