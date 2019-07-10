#ifndef PATHMANAGER_H
#define PATHMANAGER_H

#include <QLoggingCategory>
#include <string>
#include <stdio.h>
#include <iostream>
#include "common/dsv_parser/dsv.h"

Q_DECLARE_LOGGING_CATEGORY(path_manager_category)

using namespace std;

class PathManager
{
public:
    static string get_device_path();
    static string get_mst_user();
    static string get_output_dir();
    static string get_usr_share_dir();
    static string get_awesome_config();
    static string get_sudoers_d_config();
    static string get_rclua_config();
    static string get_xorg_config();
    static string get_bashrc_config();
    static string get_xinitrc_config();
    static string get_xmst_config();
    static string get_sudoers_config();
    static string get_lightdm_mst_config();
    static string get_getty_service_config();
    static string get_udev_rules_config();
    static string get_systemd_udev_config();
    static string get_vgl_config();
    static void set_user(string user_name);

    static string get_rclua_template();
    static string get_rclua4_template();
    static string get_bashrc_config_template();
    static string get_xinitrc_config_template();
    static string get_xmst_config_template();
    static string get_sudoers_config_template();
    static string get_lightdm_mst_config_template();
    static string get_getty_service_config_template();
    static string get_systemd_udev_config_template();
    static string get_vgl_config_template();

private:
    PathManager(){}
};

#endif // PATHMANAGER_H
