#include "pathmanager.h"

Q_LOGGING_CATEGORY(path_manager_category, "mst.path_manager")

string user = "";
string mstConfigFile = "/etc/mst";
string devicePath = "/dev/input/by-path/";

string sudoersD = "/etc/sudoers.d/mst";
//string output_dir = "/home/" + PathManager::get_mst_user() + "/.local/share/mst/output";
string awesome_config = "/home/" + PathManager::get_mst_user() + "/.config/awesome/rc.lua";

DSV* mst_conf = NULL;

#ifdef QT_DEBUG
    string output_dir = "/home/student/Desktop/output";
    string usrShareDir = "/home/student/Desktop/mst/templates/";
#else
    string output_dir = "/home/" + PathManager::get_mst_user() + "/.local/share/mst/output";
    string usrShareDir = "/usr/share/mst";
#endif

string PathManager::get_device_path()
{
    return devicePath;
}

string PathManager::get_mst_user()
{
    if (! mst_conf)
    {
        mst_conf = new DSV(mstConfigFile);
    }
    return mst_conf->get("user");
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
    return PathManager::get_usr_share_dir() + "/rc.lua.template";
}

string PathManager::get_rclua4_template()
{
    return PathManager::get_usr_share_dir() + "/rc.lua4.template";
}

string PathManager::get_bashrc_config_template()
{
    return PathManager::get_usr_share_dir() + "/bashrc.template";
}

string PathManager::get_xinitrc_config_template()
{
    return PathManager::get_usr_share_dir() + "/xinitrc.template";
}

string PathManager::get_xmst_config_template()
{
    return PathManager::get_usr_share_dir() + "/xmst.template";
}

string PathManager::get_sudoers_config_template()
{
    return PathManager::get_usr_share_dir() + "/sudoers.template";
}

string PathManager::get_lightdm_mst_config_template()
{
    return PathManager::get_usr_share_dir() + "/lightdm-mst.conf.template";
}

string PathManager::get_getty_service_config_template()
{
    return PathManager::get_usr_share_dir() + "/getty@.service.template";
}

string PathManager::get_systemd_udev_config_template()
{
    return PathManager::get_usr_share_dir() + "/systemd-udevd.service.template";
}

string PathManager::get_vgl_config_template()
{
    return PathManager::get_usr_share_dir() + "/vgl.sh.template";
}


