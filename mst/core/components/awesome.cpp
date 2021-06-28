/* awesome.cpp -- Awesome configuration.
 *
 * Copyright (C) 2020-2021 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2020-2021 Artyom V. Poptsov <a@gkaz.ru>b
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

#include <memory>

#include "awesome.h"
#include "../component.h"
#include "../utilites/utilites.h"
#include "../types/monitor.h"

#include "../types/template.h"
#include "core/template_manager.h"

using namespace std;
using namespace awesome;

Q_LOGGING_CATEGORY(component_awesome_category, "mst.core.component.awesome")

Awesome::Awesome(Configuration& config)
    : Component(config)
{
    /* Do nothing. */
}

void Awesome::configure()
{
    QString template_name;

    const vector<int> version = get_awesome_version();
    if (version[0] == 3)
    {
        qDebug(component_awesome_category)
                << "Using rc.lua.template for Awesome 3";
        template_name = RC_LUA_TPL_FILE;
    }
    else
    {
        qDebug(component_awesome_category)
                << "Using rc.lua.template for Awesome 4";
        template_name = RC_LUA4_TPL_FILE;
    }

    Template rclua_template
            = Template_manager::get_instance()->get_template(template_name);
    prepare_rclua_template(rclua_template);
    component_configuration.add(RC_LUA_FILE, "{{home}}/.config/awesome/rc.lua",
                                rclua_template);
}

void Awesome::install()
{
    Platform::fs_mkdir("/home/" + config.get_system_mst_user()
                       + ".config/awesome/");
    Component::install();
}

QString Awesome::get_version()
{
    return QString::fromStdString(get_awesome_raw_version());
}

void Awesome::prepare_rclua_template(Template& rclua_template)
{
    rclua_template
        .set("mst_autostart",   make_xephyr_autostart())
        .set("mst_awful_rules", make_xephyr_rules(config.get_seat_count()));
}

//// Helper procedures.

/**
 * @brief make_xephyr_autostart -- Generate Xephyr autostart commands for
 *     "rc.lua".
 *
 * Generate Lua code that starts Xephyr instances from Awesome "rc.lua" file.
 *
 * @return Generated Lua code as a string.
 */
QString awesome::make_xephyr_autostart()
{
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/mst_autostart.lua");
    tpl.set("prefix", QString::fromLocal8Bit(INSTALLATION_PREFIX));
    return tpl.substitute();
}

/**
 * @brief make_xephyr_rules -- Generate Awesome rules to arrange Xephyr
 *     instances on the screens.
 * @param sSize -- Number of seats.  This parameter affects the number of Xephyr
 *     instances.
 * @return Generated Lua code as a string.
 */
QString awesome::make_xephyr_rules(uint32_t sSize)
{
    QString result = "";
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/xephyr_rules.lua");

    for (uint32_t idx = 1; idx <= sSize; idx++)
    {
        result += tpl.set("screen_idx", QString::number(idx)).substitute();
    }
    return result;
}

/**
 * @brief get_awesome_version -- get Awesome version.
 * @throws an error message on error.
 * @return awesome version as a string.
 */
string awesome::get_awesome_raw_version()
{
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* f = popen("awesome --version", "r");
    if (f != NULL)
    {
        if (fgets(buf, BUF_SZ, f) != NULL)
        {
            return string(buf);
        }
    }
    else
    {
        const string msg = "Could not get Awesome version.";
        qCritical(component_awesome_category) << msg.c_str();
        throw msg;
    }
    return NULL;
}

/**
 * @brief awesome::get_awesome_version -- get Awesome version as a vector of
 *     strings.
 * @return awesome version as a vector of two values.
 */
vector<int> awesome::get_awesome_version()
{
    const string raw_version = get_awesome_raw_version();
    regex r1("awesome v([0-9]+).([0-9]+)*");
    smatch sm;
    if (regex_search(raw_version, sm, r1))
    {
        vector<int> result(2);
        result[0] = atoi(string(sm[1]).c_str());
        result[1] = atoi(string(sm[2]).c_str());
        return result;
    }
    else
    {
        const string msg = "Could not parse Awesome version: " + raw_version;
        qCritical(component_awesome_category) << msg.c_str();
        throw msg;
    }
}
