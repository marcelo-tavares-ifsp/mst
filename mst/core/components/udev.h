/* udev.cpp -- Udev configuration generator.
 *
 * Copyright (C) 2018-2021 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2018-2021 Artyom V. Poptsov <a@gkaz.ru>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef UDEV_H
#define UDEV_H

#include <QString>

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

namespace udev {

//// Constants.
const QString SYSTEMD_SERVICE_FILE = "systemd-udevd.service";
const QString RULES_FILE           = "99-mst.rules";

//// The main class.

class Udev : public Component
{
public:
    Udev(Configuration& config);
    void configure() override;
};


//// Helper procedures.
Template prepare_systemd_service_template();
QString prepare_udev_rules(Configuration& config);

}

#endif // UDEV_H
