#include "component_manager.h"

#include "template_manager/template_manager.h"
#include "template_manager/template.h"

#include "vgl.h"
#include "udev.h"
#include "sudo.h"
#include "display_manager.h"
#include "system.h"
#include "awesome.h"
#include "xorg.h"

Q_LOGGING_CATEGORY(components_category, "mst.components")

Component_manager::Component_manager(Configuration& config)
{
    components.push_back(new awesome::Awesome(config));
    components.push_back(new sys::System(config));
    components.push_back(new sudo::Sudo(config));
    components.push_back(new display_manager::Display_manager(config));
    components.push_back(new udev::Udev(config));
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

const vector<Component*>& Component_manager::get_components()
{
    return components;
}
