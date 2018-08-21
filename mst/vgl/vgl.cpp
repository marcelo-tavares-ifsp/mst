/* vgl.cpp -- VirtualGL interface.
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

#include <QLoggingCategory>
#include <string>
#include <stdlib.h>

#include "vgl.h"

using namespace std;

Q_LOGGING_CATEGORY(vgl_category, "mst.vgl")

Vgl::Vgl()
{

}

/**
 * @brief Vgl::configure -- Configure VirtualGL server.
 * @throws error message on an error.
 */
void Vgl::configure()
{
    static const char* cmd = "echo -e '1\nn\nn\nn\nx\n' | vglserver_config";
    if (system(cmd) > 0)
    {
        string msg = "Could not configure VirtualGL server";
        qCritical(vgl_category) << msg.c_str();
        throw msg;
    }
}

/**
 * @brief Vgl::unconfigure -- Un-configure VirtualGL server.
 * @throws error message on an error.
 */
void Vgl::unconfigure()
{
    static const char* cmd = "echo -e '2\nx\n' | vglserver_config";
    if (system(cmd) > 0)
    {
        string msg = "Could not un-configure VirtualGL server";
        qCritical(vgl_category) << msg.c_str();
        throw msg;
    }
}

//// vgl.cpp ends here.
