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

#include <fstream>

#include "udev.h"

#include "../configuration.h"
#include "core/template_manager.h"
#include "core/types/template.h"

using namespace udev;

Udev::Udev(Configuration& config) : Component("udev", config)
{
    /* Do nothing. */
}

void Udev::configure()
{
    component_configuration.add(SYSTEMD_SERVICE_FILE,
                                "/etc/systemd/system/",
                                prepare_systemd_service_template());
    component_configuration.add(RULES_FILE,
                                "/etc/udev/rules.d/",
                                Template(prepare_udev_rules(config)));
}

Template udev::prepare_systemd_service_template()
{
    Template tpl = Template_manager::get_instance()->get_template(
                SYSTEMD_SERVICE_FILE);
    return tpl;
}

QString udev::prepare_udev_rules(Configuration& config)
{
    Template tpl = Template_manager::get_instance()->get_template(RULES_FILE);
    QString result = "";

    for (int32_t idx = 0; idx < config.get_seat_count(); ++idx)
    {
        result += tpl.set("usb_device", config.get_seat(idx)->get_usb() + "/*")
                .set("prefix", QString::fromLocal8Bit(INSTALLATION_PREFIX))
                .set("seat_idx", QString::number(idx + 1 ))
                .substitute();
    }

    return result;
}
