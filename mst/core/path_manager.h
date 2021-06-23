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
    QString get_device_path();
    const QString get_mst_user();
    QString get_output_dir();
    QString get_usr_share_dir();
    QString get_sudoers_config();
    void set_user(QString user_name);

private:
    Path_manager() {};

    QString user;
    DSV* config;
    QString output_dir;
    QString usrShareDir;
    QString awesome_config;
};

#endif // PATHMANAGER_H
