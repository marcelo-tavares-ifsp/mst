#ifndef CONFIGMANAGER_H
#define CONFIGMANAGER_H

#include "configuration/configuration.h"
#include "path_manager/pathmanager.h"
#include <vector>
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
    ConfigManager(Configuration& config);
    void configure_components();
    void store_configurations(const QString& output_dir);
    const vector<Component*>& get_components();

private:
    vector<Component*> components;
};

#endif // CONFIGMANAGER_H
