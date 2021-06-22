/* vgl.cpp -- VirtualGL configuration generator.
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

#include "vgl.h"
#include "core/path_manager.h"

#include "../types/template.h"
#include "core/template_manager.h"
#include "core/platform.h"

using namespace std;
using namespace vgl;

Q_LOGGING_CATEGORY(vgl_category, "mst.core.vgl")


VGL::VGL(Configuration& config)
    : Component(config)
{
    /* Do nothing. */
}

void VGL::configure()
{
    Template tpl = Template_manager::get_instance()->get_template(VGL_SH_FILE);
    prepare_vgl_sh_template(tpl);
    component_configuration.add(VGL_SH_FILE, "/etc/bashrc.d/", tpl);
//    QString output_file = output_dir + "/" + VGL_SH_FILE;
//    tpl.substitute(output_file.toStdString());
}

/**
 * @brief VGL::enable -- Enable VirtualGL server.
 * @throws error message on an error.
 */
void VGL::enable()
{
    if (Platform::exec("echo -e '1\nn\nn\nn\nx\n' | vglserver_config") != 0) {
        throw Component_error("Could not configure VirtualGL server");
    }
}

/**
 * @brief Vgl::disable -- Disable VirtualGL server.
 * @throws error message on an error.
 */
void VGL::disable()
{
    if (Platform::exec("echo -e '2\nx\n' | vglserver_config") != 0) {
        throw Component_error("Could not un-configure VirtualGL server");
    }
    // TODO: This must be in a 'remove' method.
    Platform::fs_rm("/etc/bashrc.d/vgl.sh");
}

void VGL::prepare_vgl_sh_template(Template& tpl)
{
    const QString user = Path_manager::get_instance()->get_mst_user();
    tpl.set("user", user);
}


//// Helper procedures.


