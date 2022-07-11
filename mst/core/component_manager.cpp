/* component_manager.cpp -- MST component manager implementation.
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

#include "component_manager.h"

#include "core/template_manager.h"
#include "core/types/template.h"

#include "core/components/vgl.h"
#include "core/components/udev.h"
#include "core/components/sudo.h"
#include "core/components/display_manager.h"
#include "core/components/bash.h"
#include "core/components/system.h"
#include "core/components/awesome.h"
#include "core/components/xorg.h"
#include "core/components/pam.h"
#include "core/components/polkit.h"

Q_LOGGING_CATEGORY(component_manager_category, "mst.core.component_manager")

Component_manager::Component_manager(Configuration& config)
{
    components.push_back(new awesome::Awesome(config));
    components.push_back(new bash::Bash(config));
    components.push_back(new sys::System(config));
    components.push_back(new sudo::Sudo(config));
    components.push_back(new display_manager::Display_manager(config));
    //components.push_back(new udev::Udev(config));
    components.push_back(new polkit::Polkit(config));
    components.push_back(new vgl::VGL(config));
    components.push_back(new xorg::Xorg(config));
    components.push_back(new PAM(config));

    qInfo(component_manager_category()) << "Component versions:";
    bool error_found = false;
    for (auto component : components) {
        QString version = component->get_version();
        if (version != nullptr) {
            qInfo(component_manager_category()).noquote()
                    << "  " + component->get_name() + ": " + version;
        } else {
            qCritical(component_manager_category()).noquote()
                    << "  " + component->get_name() + ": NOT FOUND";
            std::cerr << component->get_name().toStdString()
                      << ": NOT FOUND"
                      << std::endl;
            error_found = true;
        }
    }

    if (error_found && (! config.debug_missing_components_allowed_p())) {
        throw runtime_error("Some required components are missing");
    }
}

/**
 * @brief Component_manager::configure_components -- Configure all existing
 *     components; all the configurations are stored only in the memory.
 */
void Component_manager::configure_components()
{
    for (auto component : components) {
        component->configure();
    }
}

void Component_manager::install_components()
{
    for (auto component : components) {
        component->install();
    }
}

/**
 * @brief Component_manager::enable_components -- Enable all existing
 *     components.
 */
void Component_manager::enable_components()
{
    for (auto component : components) {
        component->enable();
    }
}

/**
 * @brief Component_manager::disable_components -- Disable all existing
 *     components.
 */
void Component_manager::disable_components()
{
    for (auto component : components) {
        component->disable();
    }
}

void Component_manager::start_components()
{
    for (auto component : components) {
        component->start();
    }
}

void Component_manager::stop_components()
{
    for (auto component : components) {
        component->stop();
    }
}

/**
 * @brief Component_manager::store_configurations -- Store all configurations
 *     to the main memory.
 * @param output_dir
 */
void Component_manager::store_configurations(const QString& output_dir)
{
    for (auto component : components) {
        component->get_configuration().store(output_dir);
    }
}

void Component_manager::backup_configurations(const QString& output_dir)
{
    for (auto component : components) {
        component->get_configuration().backup(output_dir);
    }
}

void Component_manager::restore_configurations(const QString& output_dir)
{
    for (auto component : components) {
        component->get_configuration().restore(output_dir);
    }
}

const vector<Component*>& Component_manager::get_components()
{
    return components;
}
