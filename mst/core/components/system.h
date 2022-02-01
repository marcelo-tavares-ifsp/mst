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

#ifndef SYSTEM_H
#define SYSTEM_H

#include <QLoggingCategory>

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

Q_DECLARE_LOGGING_CATEGORY(component_system_category)

namespace sys {

//// Constants.
const QString GETTY_FILE  = "getty.override.conf";
const QString SEATS_CONFIG = "mst-seats";

class System : public Component
{
public:
    System(Configuration& config);
    void configure() override;
    void enable() override;
    void disable() override;
    void start() override;
    void stop() override;
    QString get_version() override;

    /**
     * @brief prepare_seat_configuration_template -- Prepare seats configuration
     *     file template.
     * @return A new seats template.
     */
    Template prepare_seat_configuration_template();

    Template prepare_getty_template();
};

}

#endif // SYSTEM_H
