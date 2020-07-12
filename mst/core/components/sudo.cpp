#include <QString>

#include "sudo.h"

#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"
#include "core/path_manager.h"

using namespace sudo;

Sudo::Sudo(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

void Sudo::configure()
{
    component_configuration.add(SUDOERS_FILE, "/etc/sudoers.d/mst",
                                prepare_sudoers_template());
}


//// Helper procedures.

Template sudo::prepare_sudoers_template()
{
    const QString user = Path_manager::get_instance()->get_mst_user();
    const QString tpl_name
            = Path_manager::get_instance()->get_sudoers_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);

    tpl.set("user", user).set("mst", QString::fromLocal8Bit(INSTALLATION_PREFIX)
                              + "/bin/mst-start-dm");
    return tpl;
}
