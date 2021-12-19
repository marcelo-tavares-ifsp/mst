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

Q_LOGGING_CATEGORY(component_pam_category, "mst.core.component.pam")

const QString PAM_ENV_CONF = "pam_env.conf";
const QString BEGIN_MARK = "BEGIN: Added by MST";
const QString END_MARK   = "END: Added by MST";
const QString COMMENT_MARK = "#";

PAM::PAM(Configuration& config) : Component("PAM", config)
{

}

void PAM::configure()
{
    component_configuration.add(PAM_ENV_CONF, "/etc/security/pam_env.conf",
                                "XDG_SEAT=seat0\n"
                                "XDG_VTNR=1\n");
}

bool PAM::installed_p(const QString &path)
{
    QFile input_file(path);
    input_file.open(QIODevice::ReadOnly);
    bool result = false;
    QString line;
    while ((line = input_file.readLine()) != nullptr) {
        if (line.contains(COMMENT_MARK + " " + BEGIN_MARK)) {
            result = true;
            break;
        }
    }
    input_file.close();
    return result;
}

void PAM::install()
{
    QString system_release = Platform::system_release();
    qInfo(component_pam_category()).noquote()
            << "System release: " << system_release;
    // ALT Education 9.1 (FalcoRusticolus)
    QRegExp alt_re("ALT.*9.*");
    if (alt_re.exactMatch(system_release)) {
        qInfo(component_pam_category())
                << "ALT 9 is found";
        const QString& path
                = component_configuration.get_installation_path(PAM_ENV_CONF);
        if (! installed_p(path)) {
            qInfo(component_pam_category())
                    << "Installing PAM configuration ...";
            QFile output_file(path);
            output_file.open(QIODevice::WriteOnly | QIODevice::Append);
            QTextStream stream(&output_file);
            stream << endl
                   << COMMENT_MARK << " " << BEGIN_MARK << endl
                   << component_configuration.get_template(PAM_ENV_CONF).substitute()
                   << COMMENT_MARK << " " << END_MARK << endl;
            output_file.close();
            qInfo(component_pam_category())
                    << "Installing PAM configuration ... done";
        } else {
            qInfo(component_pam_category())
                    << "PAM configuration is already installed";
        }
    } else {
        qInfo(component_pam_category())
                << "Skipping PAM installation";
    }
}
