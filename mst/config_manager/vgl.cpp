#include "vgl.h"
#include "../path_manager/pathmanager.h"

#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"

using namespace std;
using namespace vgl;

Q_LOGGING_CATEGORY(vgl_category, "mst.vgl")


VGL::VGL(Configuration& config)
    : Component(config)
{
    /* Do nothing. */
}

void VGL::configure()
{
    Template tpl = Template_manager::get_instance()->get_template(
                VGL_SH_FILE.toStdString());
    prepare_vgl_sh_template(tpl);
    component_configuration.add(VGL_SH_FILE, "/etc/bashrc.d/", tpl);
//    QString output_file = output_dir + "/" + VGL_SH_FILE;
//    tpl.substitute(output_file.toStdString());
}

/**
 * @brief VGL::enable -- Enable VirtualGL server.
 * @throws error message on an error.
 */
void VGL::enable()
{
    static const char* cmd = "echo -e '1\nn\nn\nn\nx\n' | vglserver_config";
    if (system(cmd) > 0)
    {
        string msg = "Could not configure VirtualGL server";
        qCritical(vgl_category) << msg.c_str();
        throw msg;
    }
}

/**
 * @brief Vgl::disable -- Disable VirtualGL server.
 * @throws error message on an error.
 */
void VGL::disable()
{
    static const char* cmd = "echo -e '2\nx\n' | vglserver_config";
    if (system(cmd) > 0)
    {
        string msg = "Could not un-configure VirtualGL server";
        qCritical(vgl_category) << msg.c_str();
        throw msg;
    }
}

void VGL::prepare_vgl_sh_template(Template& tpl)
{
    const string user = PathManager::get_instance()->get_mst_user();
    tpl.set("user", user);
}


//// Helper procedures.


