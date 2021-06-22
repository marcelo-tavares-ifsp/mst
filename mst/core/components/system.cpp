/* system.cpp -- System configuration generator.
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

#include <QString>

#include "system.h"

#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"
#include "core/path_manager.h"
#include "core/platform.h"


using namespace sys;

System::System(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

void System::configure()
{
    component_configuration.add(BASHRC_FILE,
                                "{{home}}/.bashrc",
                                prepare_bashrc_template());

    component_configuration.add(GETTY_FILE,
                                "/lib/systemd/system/getty@.service",
                                prepare_getty_template());

    component_configuration.add(SEATS_CONFIG,
                                "/etc/mst-seats",
                                prepare_seat_configuration_template());
}

void System::enable()
{
    Platform::system_set_default_runlevel("multi-user");
    Platform::exec("systemctl enable mstd");
}

void System::disable()
{
    Platform::system_set_default_runlevel("graphical");
    Platform::exec("systemctl disable mstd");
}

Template System::prepare_seat_configuration_template()
{
    Template seat_template("{{seat}} {{resolution}} {{kbd}} {{mouse}} {{usb}}");
    QString config_contents;
    for (shared_ptr<Seat> seat : config.get_seats()) {
        Resolution resolution = seat->get_monitor().get_current_resolution();
        seat_template
                .set("seat",  QString::number(seat->get_id() + 1))
                .set("resolution", resolution.to_string())
                .set("kbd",   seat->get_keyboard())
                .set("mouse", seat->get_mouse())
                .set("usb",   seat->get_usb());
        config_contents += seat_template.substitute() + "\n";
    }

    return Template(config_contents);
}

Template sys::prepare_getty_template()
{
    const QString user = Path_manager::get_instance()->get_mst_user();
    Template tpl = Template_manager::get_instance()->get_template(GETTY_FILE);
    tpl.set("user", user);
    return tpl;
}

/**
 * @brief ConfigManager::make_bashrc -- Generate ".bashrc" file for multiseat
 *     user.
 */
Template sys::prepare_bashrc_template()
{
    Template bashrc_template = Template_manager::get_instance()->get_template(
                BASHRC_FILE);
    bashrc_template.set("tty", "1");
    return bashrc_template;
}
