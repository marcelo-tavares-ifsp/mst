/* xorg.cpp -- Xorg configuration generator.
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

#include "xorg.h"
#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"

using namespace xorg;

Xorg::Xorg(Configuration& config)
    : Component("Xorg (https://www.x.org)", config)
{
    /* Do nothing. */
}

/* Config printers. */

static QString _make_monitors_section(const Configuration& config)
{
    Template_manager* template_manager = Template_manager::get_instance();
    Template monitor_tpl = template_manager->get_template("xorg/Monitor");
    Template option_tpl  = template_manager->get_template("xorg/Option");
    QString result = "";
    option_tpl.set("name", "RightOf");
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++) {
        monitor_tpl.set("index", QString::number(idx));
        if (idx > 0) {
            monitor_tpl.set("options",
                            option_tpl.set("value",
                                           "monitor" + QString::number(idx - 1))
                            .substitute());
        } else {
            monitor_tpl.set("options", "");
        }
        result += monitor_tpl.substitute();
    }
    return result;
}

static QString _make_device_section(Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/Device");
    Template option_tpl = Template_manager::get_instance()
            ->get_template("xorg/Option");
    QString monitors = "";
    for (int32_t seat_number = 1;
         seat_number <= config.get_seat_count();
         seat_number++)
    {
        QString interface = config.get_seat(seat_number)->get_monitor().get_interface();
        option_tpl.set("name",  "Monitor-" + interface);
        option_tpl.set("value", "monitor" + QString::number(seat_number));
        monitors += "    " + option_tpl
          .set("name",  "Monitor-" + interface)
          .set("value", "monitor" + QString::number(seat_number))
          .substitute() + "\n";
    }
    tpl.set("monitors", monitors);
    return tpl.substitute();
}

static QString _make_screen_section(Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/Screen");
    tpl.set("depth", "24");
    QString result = "";
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++) {
        result += tpl.set("index", QString::number(idx)).substitute();
    }

    return result;
}

static QString _make_layout_section(Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/ServerLayout");
    QString result = "";
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++) {
        result += tpl.set("seat_index", QString::number(idx)).substitute();
    }
    return result;
}

void Xorg::configure()
{
    QString xorg_conf = _make_monitors_section(config)
            + _make_device_section(config)
            + _make_screen_section(config)
            + _make_layout_section(config);

    component_configuration.add(XORG_FILE,
                                "/etc/X11/xorg.conf",
                                Template(xorg_conf));

    component_configuration.add(XINIT_RC_FILE,
                                "{{home}}/.xinitrc",
                                xorg::prepare_xinitrc_template());
}

Template xorg::prepare_xinitrc_template()
{
    return Template_manager::get_instance()->get_template(XINIT_RC_FILE);
}

QString Xorg::get_version()
{
    QVector<QString> result = platform::popen_read("vglclient",
                                                   QStringList() << "-v",
                                                   QProcess::StandardError);
    return result[1];
}
