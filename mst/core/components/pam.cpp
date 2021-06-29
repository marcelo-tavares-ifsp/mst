/* pam.cpp -- PAM configuration.
 *
 * Copyright (C) 2021 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2021 Artyom V. Poptsov <a@gkaz.ru>b
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

#include "pam.h"

const QString PAM_ENV_CONF = "pam_env.conf";

PAM::PAM(Configuration& config) : Component(config)
{

}

void PAM::configure()
{
    component_configuration.add(PAM_ENV_CONF, "/etc/security/pam_env.conf",
                                "XDG_SEAT=seat0\n"
                                "XDG_VTNR=1\n");
}

void PAM::install()
{
    const QString& path
            = component_configuration.get_installation_path(PAM_ENV_CONF);
    QFile output_file(path);
    output_file.open(QIODevice::WriteOnly | QIODevice::Append);
    QTextStream stream(&output_file);
    stream << endl
           << "# BEGIN: Added by MST" << endl
           << component_configuration.get_template(PAM_ENV_CONF).substitute()
           << "# END: Added by MST" << endl;
    output_file.close();
}
