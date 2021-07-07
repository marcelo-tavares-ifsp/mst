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

#ifndef XORG_CONFIG_H
#define XORG_CONFIG_H

#include "core/configuration.h"
#include "../types/template.h"
#include "iostream"
#include <QLoggingCategory>

#include "../component.h"

namespace xorg {

//// Constants.

static const QString XORG_FILE = "xorg.conf";
static const QString XINIT_RC_FILE    = "xinitrc";

class Xorg : public Component
{
public:
    Xorg(Configuration& config);
    void configure() override;
    QString get_version() override;
};


//// Helper procedures.
Template prepare_xinitrc_template();

}

#endif // XORG_CONFIG_H
