Name:    mst
Version: 0.1.0
Release: 1
Summary: Multi-Seat Tool (MST)
License: GPLv3+
URL:     https://gitlab.nntc.nnov.ru/az/mst
Source0: mst1.0.0.tar.gz
Group:   System/Base

Requires: awesome, unclutter, xorg-xephyr, lightdm, bash

Provides: mst

%description
Multi-Seat Tool.

%prep

# %autosetup

%build

%configure

%install

%post


