#include <QString>

#include "path_manager.h"

using namespace std;

Q_LOGGING_CATEGORY(path_manager_category, "mst.core.path_manager")

string devicePath = "/dev/input/by-path/";

string sudoersD = "/etc/sudoers.d/mst";

void Path_manager::set_config(DSV* config)
{
    this->config = config;
    string mst_user = get_mst_user().toStdString();
    awesome_config = "/home/" + mst_user + "/.config/awesome/rc.lua";

    output_dir = "/home/" + mst_user + "/.local/share/mst/output";
    usrShareDir = "/usr/share/mst";
}

string Path_manager::get_device_path()
{
    return devicePath;
}

const QString Path_manager::get_mst_user()
{
    return QString::fromStdString(config->get("user"));
}

string Path_manager::get_output_dir()
{
    return output_dir;
}

string Path_manager::get_usr_share_dir()
{
    return usrShareDir;
}

string Path_manager::get_sudoers_config()
{
    return Path_manager::get_output_dir() + "/sudoers";
}

void Path_manager::set_user(string user_name)
{
    user = user_name;
}
