#include <QString>

#include "pathmanager.h"

Q_LOGGING_CATEGORY(path_manager_category, "mst.path_manager")

string devicePath = "/dev/input/by-path/";

string sudoersD = "/etc/sudoers.d/mst";

void PathManager::set_config(DSV* config)
{
    this->config = config;
    string mst_user = get_mst_user().toStdString();
    awesome_config = "/home/" + mst_user + "/.config/awesome/rc.lua";

    output_dir = "/home/" + mst_user + "/.local/share/mst/output";
    usrShareDir = "/usr/share/mst";
}

string PathManager::get_device_path()
{
    return devicePath;
}

const QString PathManager::get_mst_user()
{
    return QString::fromStdString(config->get("user"));
}

string PathManager::get_output_dir()
{
    return output_dir;
}

string PathManager::get_usr_share_dir()
{
    return usrShareDir;
}

string PathManager::get_awesome_config()
{
    return awesome_config;
}

string PathManager::get_sudoers_d_config()
{
    return sudoersD;
}

string PathManager::get_rclua_config()
{
    return PathManager::get_output_dir() + "/rc.lua";
}

string PathManager::get_xorg_config()
{
    return PathManager::get_output_dir() + "/xorg.conf";
}

string PathManager::get_bashrc_config()
{
    return PathManager::get_output_dir() + "/.bashrc";
}

string PathManager::get_xinitrc_config()
{
    return PathManager::get_output_dir() + "/.xinitrc";
}

string PathManager::get_xmst_config()
{
    return PathManager::get_output_dir() + "/.xmst";
}

string PathManager::get_sudoers_config()
{
    return PathManager::get_output_dir() + "/sudoers";
}

string PathManager::get_lightdm_mst_config()
{
    return PathManager::get_output_dir() + "/lightdm-mst.conf";
}

string PathManager::get_getty_service_config()
{
    return PathManager::get_output_dir() + "/getty@.service";
}

string PathManager::get_udev_rules_config()
{
    return PathManager::get_output_dir() + "/99-mst.rules";
}

string PathManager::get_systemd_udev_config()
{
    return PathManager::get_output_dir() + "/systemd-udevd.service";
}

string PathManager::get_vgl_config()
{
    return PathManager::get_output_dir() + "/vgl.sh";
}

void PathManager::set_user(string user_name)
{
    user = user_name;
}

///////////////Templates paths/////////////////////////////////////////////////

string PathManager::get_rclua_template()
{
    return "rc.lua";
}

string PathManager::get_rclua4_template()
{
    return "rc.lua4";
}

string PathManager::get_awesome_xephyr_rules_template()
{
    return "awesome/xephyr_rules";
}

string PathManager::get_bashrc_config_template()
{
    return "bashrc";
}

string PathManager::get_xinitrc_config_template()
{
    return "xinitrc";
}

string PathManager::get_xmst_config_template()
{
    return "xmst";
}

QString PathManager::get_sudoers_config_template()
{
    return "sudoers";
}

string PathManager::get_lightdm_mst_config_template()
{
    return "lightdm-mst.conf";
}

string PathManager::get_getty_service_config_template()
{
    return "getty@.service";
}

string PathManager::get_systemd_udev_config_template()
{
    return "systemd-udevd.service";
}

string PathManager::get_vgl_config_template()
{
    return "vgl.sh";
}


