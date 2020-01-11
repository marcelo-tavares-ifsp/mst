#ifndef CONFIGMANAGER_H
#define CONFIGMANAGER_H

#include <vector>
#include <QLoggingCategory>

#include "component.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(components_category)

class Component_manager
{
public:
    Component_manager(Configuration& config);
    void configure_components();
    void store_configurations(const QString& output_dir);
    const vector<Component*>& get_components();

private:
    vector<Component*> components;
};

#endif // CONFIGMANAGER_H
