#ifndef PATHMANAGER_H
#define PATHMANAGER_H

#include <QLoggingCategory>
#include <string>
#include <stdio.h>
#include <iostream>
#include "dsv_parser/dsv.h"

Q_DECLARE_LOGGING_CATEGORY(path_manager_category)

class Path_manager
{
public:
    static Path_manager* get_instance()
    {
        static Path_manager* instance = NULL;
        if (instance == NULL) {
            instance = new Path_manager();
        }
        return instance;
    }
    void set_config(DSV* config);
    std::string get_device_path();
    const QString get_mst_user();
    std::string get_output_dir();
    std::string get_usr_share_dir();
    std::string get_sudoers_config();
    void set_user(std::string user_name);

private:
    Path_manager() {};

    std::string user;
    DSV* config;
    std::string output_dir;
    std::string usrShareDir;
    std::string awesome_config;
};

#endif // PATHMANAGER_H
