/* config.cpp -- MST configuration.
 *
 * Copyright (C) 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include "config.h"

#include "dsv.h"

const string Config::MST_CONFIG_FILE  = "/etc/mst";
Config* Config::instance = NULL;

Config::Config()
{

}

Config* Config::get_instance()
{
    if (! instance)
    {
        instance = new Config();
        instance->mst_conf = new DSV(MST_CONFIG_FILE);
        instance->mst_user = instance->mst_conf->get("user");
        instance->usr_share_dir = "/usr/share/mst";
        instance->output_dir
                = "/home/" + instance->mst_user + "/.local/share/mst/output";

        instance->awesome_config
                = "/home/" + instance->mst_user + "/.config/awesome/rc.lua";
        instance->sudoers_config = "/etc/sudoers.d/mst";
    }

    return instance;
}

const string Config::get_mst_user() const
{
    return mst_user;
}

const string Config::get_output_dir() const
{
    return output_dir;
}

const string Config::get_usr_share_dir() const
{
    return usr_share_dir;
}

const string Config::get_awesome_config() const
{
    return awesome_config;
}

const string Config::get_sudoers_config() const
{
    return sudoers_config;
}
