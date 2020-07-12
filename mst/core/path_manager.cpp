#include <QString>

#include "path_manager.h"

Q_LOGGING_CATEGORY(path_manager_category, "mst.path_manager")

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

string Path_manager::get_awesome_config()
{
    return awesome_config;
}

string Path_manager::get_sudoers_d_config()
{
    return sudoersD;
}

string Path_manager::get_rclua_config()
{
    return Path_manager::get_output_dir() + "/rc.lua";
}

string Path_manager::get_xorg_config()
{
    return Path_manager::get_output_dir() + "/xorg.conf";
}

string Path_manager::get_bashrc_config()
{
    return Path_manager::get_output_dir() + "/.bashrc";
}

string Path_manager::get_xinitrc_config()
{
    return Path_manager::get_output_dir() + "/.xinitrc";
}

string Path_manager::get_xmst_config()
{
    return Path_manager::get_output_dir() + "/.xmst";
}

string Path_manager::get_sudoers_config()
{
    return Path_manager::get_output_dir() + "/sudoers";
}

string Path_manager::get_lightdm_mst_config()
{
    return Path_manager::get_output_dir() + "/lightdm-mst.conf";
}

string Path_manager::get_getty_service_config()
{
    return Path_manager::get_output_dir() + "/getty@.service";
}

string Path_manager::get_udev_rules_config()
{
    return Path_manager::get_output_dir() + "/99-mst.rules";
}

string Path_manager::get_systemd_udev_config()
{
    return Path_manager::get_output_dir() + "/systemd-udevd.service";
}

string Path_manager::get_vgl_config()
{
    return Path_manager::get_output_dir() + "/vgl.sh";
}

void Path_manager::set_user(string user_name)
{
    user = user_name;
}

///////////////Templates paths/////////////////////////////////////////////////

string Path_manager::get_rclua_template()
{
    return "rc.lua";
}

string Path_manager::get_rclua4_template()
{
    return "rc.lua4";
}

string Path_manager::get_awesome_xephyr_rules_template()
{
    return "awesome/xephyr_rules";
}

string Path_manager::get_bashrc_config_template()
{
    return "bashrc";
}

string Path_manager::get_xinitrc_config_template()
{
    return "xinitrc";
}

string Path_manager::get_xmst_config_template()
{
    return "xmst";
}

QString Path_manager::get_sudoers_config_template()
{
    return "sudoers";
}

string Path_manager::get_lightdm_mst_config_template()
{
    return "lightdm-mst.conf";
}

string Path_manager::get_getty_service_config_template()
{
    return "getty@.service";
}

string Path_manager::get_systemd_udev_config_template()
{
    return "systemd-udevd.service";
}

string Path_manager::get_vgl_config_template()
{
    return "vgl.sh";
}


