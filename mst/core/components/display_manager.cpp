/* display_manager.cpp -- Display manager (LightDM) configuration generator.
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

#include "display_manager.h"

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"
#include "core/platform.h"

Q_LOGGING_CATEGORY(display_manager_category, "mst.core.component.dm")

using namespace display_manager;

Display_manager::Display_manager(Configuration& config) : Component(config)
{

}

void Display_manager::configure()
{
    QString target_file = "/etc/lightdm/" + LIGHTDM_FILE;
    QString version = get_version();
    qInfo(display_manager_category())
            << "LightDM version:" << version;
    // lightdm 1.30.0
    QRegExp re("lightdm ([0-9]+)\\.([0-9]+)\\.([0-9]+)");
    re.exactMatch(version);
    int major = re.cap(1).toInt();
    int minor = re.cap(2).toInt();
    int patch = re.cap(3).toInt();

    qDebug(display_manager_category())
            << "  Major:" << major
            << "  Minor:" << minor
            << "  Patch:" << patch;

    Template tpl;

    if ((major >= 1) && (minor >= 15)) {
        // [SeatDefaults] deprecated in favour of [Seat:*]
        // See <https://github.com/canonical/lightdm/blob/master/NEWS>
        tpl = Template_manager::get_instance()->get_template(LIGHTDM_FILE);
    } else {
        qInfo(display_manager_category())
                << "Using old configuration file template";
        tpl = Template_manager::get_instance()->get_template(LIGHTDM_OLD_FILE);
    }

    component_configuration.add(LIGHTDM_FILE, target_file, tpl);
}

void Display_manager::enable() {
     // Do nothing.
}

QString Display_manager::get_version()
{
    QVector<QString> result = platform::popen_read("lightdm",
                                                   QStringList() << "--version",
                                                   QProcess::StandardError);
    return result[0];
}
