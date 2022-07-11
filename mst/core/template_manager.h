/* template_manager.h -- MST template manager class.
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

#ifndef TEMPLATE_MANAGER_H
#define TEMPLATE_MANAGER_H

#include "types/template.h"

using namespace std;

class Template_manager
{
public:
    static const QString TEMPLATE_FILE_EXTENSION;

    Template get_template(const QString& name);

    static Template_manager* get_instance();
    void set_template_dir(const QString& templates_dir);

private:
    QString templates_dir;
};

#endif // TEMPLATE_MANAGER_H
