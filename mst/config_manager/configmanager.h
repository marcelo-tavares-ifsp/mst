#ifndef CONFIGMANAGER_H
#define CONFIGMANAGER_H

#include "configuration/configuration.h"
#include "path_manager/pathmanager.h"
#include "xorg.h"
#include "common/utilites/utilites.h"
#include "iostream"
#include <fstream>
#include <QLoggingCategory>

#include "awesome.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(config_manager_category)

class ConfigManager
{
public:
    friend ostream& operator << (ostream& os, const Configuration& config);
    static void make_rc_lua(Configuration& config);
    static void make_xorg(Configuration& config);
    static void configure_system(Configuration& config);
    static void make_sudoers(Configuration& config);
    static void make_lightdm_conf(Configuration& config);
    static void configure_udev(Configuration& config);
    static void make_vgl(Configuration& config);

private:
    ConfigManager(){}
};

#endif // CONFIGMANAGER_H
