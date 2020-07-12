#include <QString>

#include "system.h"

#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"
#include "core/path_manager.h"

using namespace sys;

System::System(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

void System::configure()
{
    component_configuration.add(BASHRC_FILE,
                                "{{home}}/.bashrc",
                                prepare_bashrc_template());

    component_configuration.add(GETTY_FILE,
                                "/lib/systemd/system/getty@.service",
                                prepare_getty_template());
}

Template sys::prepare_getty_template()
{
    const QString user = PathManager::get_instance()->get_mst_user();
    Template tpl = Template_manager::get_instance()->get_template(GETTY_FILE);
    tpl.set("user", user);
    return tpl;
}

/**
 * @brief ConfigManager::make_bashrc -- Generate ".bashrc" file for multiseat
 *     user.
 */
Template sys::prepare_bashrc_template()
{
    Template bashrc_template = Template_manager::get_instance()->get_template(
                BASHRC_FILE);
    bashrc_template.set("tty", "1");
    return bashrc_template;
}
