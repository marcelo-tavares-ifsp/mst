#include "vgl.h"

using namespace std;

Q_LOGGING_CATEGORY(vgl_category, "mst.vgl")


VGL::VGL()
{

}

/**
 * @brief VGL::configure -- Configure VirtualGL server.
 * @throws error message on an error.
 */
void VGL::configure()
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
 * @brief Vgl::unconfigure -- Un-configure VirtualGL server.
 * @throws error message on an error.
 */
void VGL::unconfigure()
{
    if (CommandManager::config_vgl())
    {
        string msg = "Could not un-configure VirtualGL server";
        qCritical(vgl_category) << msg.c_str();
        throw msg;
    }
}
