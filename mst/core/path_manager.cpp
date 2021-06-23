#include <QString>

#include "path_manager.h"

using namespace std;

Q_LOGGING_CATEGORY(path_manager_category, "mst.core.path_manager")

QString devicePath = "/dev/input/by-path/";

QString sudoersD = "/etc/sudoers.d/mst";

void Path_manager::set_config(DSV* config)
{
    this->config = config;
    QString mst_user = get_mst_user();
    awesome_config = "/home/" + mst_user + "/.config/awesome/rc.lua";

    output_dir = "/home/" + mst_user + "/.local/share/mst/output";
    usrShareDir = "/usr/share/mst";
}

QString Path_manager::get_device_path()
{
    return devicePath;
}

const QString Path_manager::get_mst_user()
{
    return QString::fromStdString(config->get("user"));
}

QString Path_manager::get_output_dir()
{
    return output_dir;
}

QString Path_manager::get_usr_share_dir()
{
    return usrShareDir;
}

QString Path_manager::get_sudoers_config()
{
    return Path_manager::get_output_dir() + "/sudoers";
}

void Path_manager::set_user(QString user_name)
{
    user = user_name;
}
