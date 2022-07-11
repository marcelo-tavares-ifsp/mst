/* template_manager.cpp -- MST template manager implementation.
 *
 * Copyright (C) 2018-2022 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2018-2022 Artyom V. Poptsov <a@gkaz.ru>
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

#include "template_manager.h"

#include <QFile>

using namespace std;

const QString Template_manager::TEMPLATE_FILE_EXTENSION = ".template";

void Template_manager::set_template_dir(const QString& templates_dir)
{
    this->templates_dir = templates_dir;
}

/**
 * @brief Template_manager::get_template -- Get a Template instance by a name.
 * @param name -- Template name to search.
 * @return A new Template instance.
 */
Template Template_manager::get_template(const QString& name)
{
    QString file_name = templates_dir + "./" + name
            + Template_manager::TEMPLATE_FILE_EXTENSION;
    QFile file(file_name);
    return Template(file);
}

/**
 * @brief Template_manager::get_instance -- Get the Template_manager instance.
 * @return -- A static Template_manager instance.
 */
Template_manager* Template_manager::get_instance()
{
    static Template_manager* instance = NULL;
    if (instance == NULL) {
        instance = new Template_manager();
    }
    return instance;
}
