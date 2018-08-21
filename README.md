![MST Logo](doc/logo.png)

# MST
Multi-Seat Tool

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
* Awesome
* unclutter
* Xephyr
* lightdm
* bash
* libqt5-core
* libudev-devel
* virtualgl

## Installation

An RPM package for the project can be built as follows:
```
$ make -j4 rpm
```
(You should specify apropriate `-j` option according to the number 
of available CPU cores to speed up the building process.)

Installation from the RPM package:

```
$ apt-get install awesome unclutter xorg-xephyr libqt5-core libudev-devel
$ rpm -i mst-1.0.0-1.x86_64.rpm
```
