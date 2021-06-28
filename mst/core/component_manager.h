#ifndef CONFIGMANAGER_H
#define CONFIGMANAGER_H

#include <vector>
#include <QLoggingCategory>
#include <QString>

#include "component.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(component_manager_category)

class Component_manager
{
public:
    Component_manager(Configuration& config);
    void configure_components();
    void install_components();
    void enable_components();
    void disable_components();
    void stop_components();
    void start_components();
    void store_configurations(const QString& output_dir);
    void backup_configurations(const QString& output_dir);
    void restore_configurations(const QString& output_dir);
    const vector<Component*>& get_components();

private:
    vector<Component*> components;
};

#endif // CONFIGMANAGER_H
