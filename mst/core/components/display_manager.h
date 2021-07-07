/* display_manager.h -- Display manager (LightDM) configuration generator.
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

#ifndef DISPLAY_MANAGER_H
#define DISPLAY_MANAGER_H

#include <string>
#include <stdlib.h>
#include <QLoggingCategory>

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

Q_DECLARE_LOGGING_CATEGORY(display_manager_category)

namespace display_manager {

//// Constants.

const QString LIGHTDM_FILE = "lightdm-mst.conf";

class Display_manager : public Component
{
public:
    Display_manager(Configuration& config);

    void configure() override;
    void enable() override;
    QString get_version() override;
};


//// Helper procedures.
Template prepare_lightdm_template();

}

#endif // DISPLAY_MANAGER_H
