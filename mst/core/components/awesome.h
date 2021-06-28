/* awesome.h -- Awesome configuration.
 *
 * Copyright (C) 2020 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2020 Artyom V. Poptsov <a@gkaz.ru>b
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

#ifndef AWESOME_CONFIG_H
#define AWESOME_CONFIG_H

#include <string>
#include <vector>
#include <sstream>
#include <memory>

#include "../configuration.h"
#include "../types/template.h"
#include "../component.h"
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(component_awesome_category)

namespace awesome {

//// Constants.

static const QString RC_LUA_FILE      = "rc.lua";
static const QString RC_LUA_TPL_FILE  = "rc.lua";
static const QString RC_LUA4_TPL_FILE = "rc.lua.4";

//// The main class.

class Awesome : public Component
{
public:
    Awesome(Configuration& config);
    void configure() override;
    QString get_version() override;
    void install() override;

    void prepare_rclua_template(Template& rclua_template);
};

//// Helper procedures.

extern QString make_xephyr_autostart();
extern QString make_xephyr_rules(uint32_t sSize);

extern std::string get_awesome_raw_version();
extern std::vector<int> get_awesome_version();

}

#endif // AWESOME_CONFIG_H
