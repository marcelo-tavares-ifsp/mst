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

#ifndef SUDO_H
#define SUDO_H

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

namespace sudo {

static const QString SUDOERS_FILE = "sudoers";

class Sudo : public Component
{
public:
    Sudo(Configuration& config);
    void configure() override;
    void disable() override;
    void install() override;

    Template prepare_sudoers_template();
};

}

#endif // SUDO_H
