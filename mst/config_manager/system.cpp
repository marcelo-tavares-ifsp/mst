#include "system.h"

#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"
#include "../path_manager/pathmanager.h"

using namespace sys;

System::System(Configuration& config) : Component(config)
{
    config_files[BASHRC_FILE] = "{{home}}/.bashrc";
    config_files[GETTY_FILE]  = "/lib/systemd/system/getty@.service";
}

void System::configure(const QString &output_dir)
{
    QString output_file = output_dir + "/" + BASHRC_FILE;
    Template tpl = prepare_bashrc_template();
    tpl.substitute(output_file.toStdString());

    output_file = output_dir + "/" + GETTY_FILE;
    tpl = prepare_getty_template();
    tpl.substitute(output_file.toStdString());
}


Template sys::prepare_getty_template()
{
    const string user = PathManager::get_instance()->get_mst_user();
    Template tpl = Template_manager::get_instance()->get_template(
                GETTY_FILE.toStdString());
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
                BASHRC_FILE.toStdString());
    bashrc_template.set("tty", "1");
    return bashrc_template;
}
