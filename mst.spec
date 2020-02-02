Name:    mst
Version: 0.1.0
Release: 1
Summary: Multi-Seat Tool (MST)
License: GPLv3+
URL:     https://gitlab.com/gkaz/mst
Source0: %{name}%{version}.tar.gz
Group:   System/Base

Requires: awesome, unclutter, xorg-xephyr, lightdm, bash
Requires: libqt5-core, libudev-devel, virtualgl

Provides: mst

%description
Multi-Seat Tool.

%prep

%setup -q -n %{name}%{version} -a0

%build
cd mst
qmake-qt5
make -j4

%install
mkdir -p $RPM_BUILD_ROOT/usr/local/bin
mkdir -p $RPM_BUILD_ROOT/usr/share/mst/awesome
mkdir -p $RPM_BUILD_ROOT/etc
install -m0755 mst/mst $RPM_BUILD_ROOT/usr/local/bin/mst
install -m0644 templates/*.template $RPM_BUILD_ROOT/usr/share/mst/
install -m6444 templates/awesome/*.template $RPM_BUILD_ROOT/usr/share/mst/awesome/
install -m0755 scripts/*   $RPM_BUILD_ROOT/usr/local/bin/
install -m0644 etc/mst     $RPM_BUILD_ROOT/etc

%files
/usr/local/bin/*
/usr/share/mst/*
/etc/mst

%post
useradd -G wheel -m multiseat



