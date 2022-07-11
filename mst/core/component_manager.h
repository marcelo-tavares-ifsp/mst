/* component_manager.h -- MST component manager.
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

#ifndef CONFIGMANAGER_H
#define CONFIGMANAGER_H

#include <vector>
#include <QLoggingCategory>
#include <QString>

#include "component.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(component_manager_category)

class Component_manager
{
public:
    Component_manager(Configuration& config);
    void configure_components();
    void install_components();
    void enable_components();
    void disable_components();
    void stop_components();
    void start_components();
    void store_configurations(const QString& output_dir);
    void backup_configurations(const QString& output_dir);
    void restore_configurations(const QString& output_dir);
    const vector<Component*>& get_components();

private:
    vector<Component*> components;
};

#endif // CONFIGMANAGER_H
