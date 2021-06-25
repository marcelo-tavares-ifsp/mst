#include <QString>

#include "path_manager.h"

using namespace std;

Q_LOGGING_CATEGORY(path_manager_category, "mst.core.path_manager")

void Path_manager::set_config(DSV* config)
{
    this->config = config;
    QString mst_user = get_mst_user();

    output_dir = "/home/" + mst_user + "/.local/share/mst/output";
}

const QString Path_manager::get_mst_user()
{
    return QString::fromStdString(config->get("user"));
}

QString Path_manager::get_output_dir()
{
    return output_dir;
}
