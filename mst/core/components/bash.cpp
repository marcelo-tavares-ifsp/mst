/* bash.cpp -- Bash configuration.
 *
 * Copyright (C) 2020-2022 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2020-2022 Artyom V. Poptsov <a@gkaz.ru>b
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

#include "bash.h"

Q_LOGGING_CATEGORY(component_bash_category, "mst.core.component.bash")

using namespace bash;

Bash::Bash(Configuration& config) : Component("Bash", config)
{

}

void Bash::configure()
{
    component_configuration.add(BASH_PROFILE_FILE,
                                "{{home}}/.bash_profile",
                                prepare_bash_profile_template());
}

bool Bash::installed_p(const QString &path)
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

void Bash::install()
{
    const QString& path
            = component_configuration.get_installation_path(BASH_PROFILE_FILE);
    if (! installed_p(path)) {
        qInfo(component_bash_category())
                << "Installing Bash configuration ...";
        QFile output_file(path);
        output_file.open(QIODevice::WriteOnly | QIODevice::Append);
        QTextStream stream(&output_file);
        stream << endl
               << COMMENT_MARK << " " << BEGIN_MARK << endl
               << component_configuration.get_template(BASH_PROFILE_FILE)
                  .substitute()
               << COMMENT_MARK << " " << END_MARK << endl;
        output_file.close();
        qInfo(component_bash_category())
                << "Installing Bash configuration ... done";
    } else {
        qInfo(component_bash_category())
                << "Bash configuration is already installed";
    }
}

/**
 * @brief ConfigManager::make_bashrc -- Generate ".bashrc" file for multiseat
 *     user.
 */
Template bash::prepare_bash_profile_template()
{
    Template bashrc_template = Template_manager::get_instance()->get_template(
                BASH_PROFILE_FILE);
    bashrc_template.set("tty", "1");
    return bashrc_template;
}


QString Bash::get_version() {
    try {
        QVector<QString> result = platform::popen_read(
                    "bash",
                    QStringList() << "--version",
                    QProcess::StandardError);
    return (result.length() > 2) ? result[0] : nullptr;
    }  catch (Platform_exception& e) {
        return nullptr;
    }
}
