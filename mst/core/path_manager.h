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
    const QString get_mst_user();
    QString get_output_dir();

private:
    Path_manager() {};

    QString user;
    DSV* config;
    QString output_dir;
};

#endif // PATHMANAGER_H
