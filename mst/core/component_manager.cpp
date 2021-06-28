#include "component_manager.h"

#include "core/template_manager.h"
#include "core/types/template.h"

#include "core/components/vgl.h"
#include "core/components/udev.h"
#include "core/components/sudo.h"
#include "core/components/display_manager.h"
#include "core/components/system.h"
#include "core/components/awesome.h"
#include "core/components/xorg.h"

Q_LOGGING_CATEGORY(component_manager_category, "mst.core.component_manager")

Component_manager::Component_manager(Configuration& config)
{
    components.push_back(new awesome::Awesome(config));
    components.push_back(new sys::System(config));
    components.push_back(new sudo::Sudo(config));
    components.push_back(new display_manager::Display_manager(config));
    //components.push_back(new udev::Udev(config));
    components.push_back(new vgl::VGL(config));
    components.push_back(new xorg::Xorg(config));
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
