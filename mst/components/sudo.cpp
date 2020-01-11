#include <QString>

#include "sudo.h"

#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"
#include "../path_manager/pathmanager.h"

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
    const QString user = PathManager::get_instance()->get_mst_user();
    const string out_file_name
            = PathManager::get_instance()->get_sudoers_config();
    const string tpl_name
            = PathManager::get_instance()->get_sudoers_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);

    tpl.set("user", user.toStdString())
            .set("mst", "/usr/local/bin/mst-start-dm");
    return tpl;
}
