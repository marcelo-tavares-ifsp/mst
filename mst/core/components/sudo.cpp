/* sudo.cpp -- Sudo (sudoers) configuration generator.
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

#include <core/platform.h>

#include "sudo.h"

#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"

using namespace sudo;

Sudo::Sudo(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

void Sudo::configure()
{
    component_configuration.add(SUDOERS_FILE, "/etc/sudoers.d/mst",
                                prepare_sudoers_template());
}

void Sudo::disable()
{
    Platform::fs_rm(QString::fromStdString("/etc/sudoers.d/mst"));
}

//// Helper procedures.

Template Sudo::prepare_sudoers_template()
{
    const QString user = config.get_system_mst_user();
    Template tpl = Template_manager::get_instance()->get_template("sudoers");

    tpl.set("user", user).set("mst", QString::fromLocal8Bit(INSTALLATION_PREFIX)
                              + "/bin/mst-start-dm");
    return tpl;
}
