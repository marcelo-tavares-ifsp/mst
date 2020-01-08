#include "sudo.h"

#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"
#include "../path_manager/pathmanager.h"

using namespace sudo;

Sudo::Sudo(Configuration& config) : Component(config)
{
    config_files[SUDOERS_FILE] = "/etc/sudoers.d/mst";
}

void Sudo::configure(const QString& output_dir)
{
    QString out_file_name = output_dir + "/" + SUDOERS_FILE;
    Template tpl = prepare_sudoers_template();
    tpl.substitute(out_file_name.toStdString());
}


//// Helper procedures.

Template sudo::prepare_sudoers_template()
{
    const string user = PathManager::get_instance()->get_mst_user();
    const string out_file_name
            = PathManager::get_instance()->get_sudoers_config();
    const string tpl_name
            = PathManager::get_instance()->get_sudoers_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);

    tpl.set("user", user).set("mst", "/usr/local/bin/mst-start-dm");
    return tpl;
}
