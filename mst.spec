Name:    mst
Version: 1.0.0
Release: 1
Summary: Multi-Seat Tool (MST)
License: GPLv3+
URL:     https://gitlab.nntc.nnov.ru/az/mst
Source0: %{name}%{version}.tar.gz
Group:   System/Base

Requires: awesome, unclutter, xorg-xephyr, lightdm, bash

Provides: mst

%description
Multi-Seat Tool.

%prep

%setup -q -n %{name}%{version} -a0

%build
qmake-qt5
make -j4

%install
mkdir -p $RPM_BUILD_ROOT/usr/local/bin
install mst/mst $RPM_BUILD_ROOT/usr/local/bin/software

%files

%post


