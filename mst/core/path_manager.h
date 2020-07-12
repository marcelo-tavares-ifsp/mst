#ifndef PATHMANAGER_H
#define PATHMANAGER_H

#include <QLoggingCategory>
#include <string>
#include <stdio.h>
#include <iostream>
#include "dsv_parser/dsv.h"

Q_DECLARE_LOGGING_CATEGORY(path_manager_category)

using namespace std;

class PathManager
{
public:
    static PathManager* get_instance()
    {
        static PathManager* instance = NULL;
        if (instance == NULL) {
            instance = new PathManager();
        }
        return instance;
    }
    void set_config(DSV* config);
    string get_device_path();
    const QString get_mst_user();
    string get_output_dir();
    string get_usr_share_dir();
    string get_awesome_config();
    string get_sudoers_d_config();
    string get_rclua_config();
    string get_awesome_xephyr_rules_template();
    string get_xorg_config();
    string get_bashrc_config();
    string get_xinitrc_config();
    string get_xmst_config();
    string get_sudoers_config();
    string get_lightdm_mst_config();
    string get_getty_service_config();
    string get_udev_rules_config();
    string get_systemd_udev_config();
    string get_vgl_config();
    void set_user(string user_name);

    string get_rclua_template();
    string get_rclua4_template();
    string get_bashrc_config_template();
    string get_xinitrc_config_template();
    string get_xmst_config_template();
    QString get_sudoers_config_template();
    string get_lightdm_mst_config_template();
    string get_getty_service_config_template();
    string get_systemd_udev_config_template();
    string get_vgl_config_template();

private:
    PathManager() {};

    string user;
    DSV* config;
    string output_dir;
    string usrShareDir;
    string awesome_config;
};

#endif // PATHMANAGER_H
