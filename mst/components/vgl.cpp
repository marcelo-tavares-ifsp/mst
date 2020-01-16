#include "vgl.h"
#include "../path_manager/pathmanager.h"

#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"
#include "../platform/platform.h"

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
    try {
        Platform::exec("echo -e '1\nn\nn\nn\nx\n' | vglserver_config");
    } catch (Platform_exception& e) {
        throw Component_error("Could not configure VirtualGL server");
    }
}

/**
 * @brief Vgl::disable -- Disable VirtualGL server.
 * @throws error message on an error.
 */
void VGL::disable()
{
    try {
        Platform::exec("echo -e '2\nx\n' | vglserver_config");
    } catch (Platform_exception& e) {
        throw Component_error("Could not un-configure VirtualGL server");
    }
}

void VGL::prepare_vgl_sh_template(Template& tpl)
{
    const QString user = PathManager::get_instance()->get_mst_user();
    tpl.set("user", user.toStdString());
}


//// Helper procedures.

