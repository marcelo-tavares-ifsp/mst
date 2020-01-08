#ifndef COMPONENT_H
#define COMPONENT_H

#include <QMap>
#include <QString>

#include "../configuration/configuration.h"

class Component
{
public:
    Component(Configuration& config);
    virtual void configure(const QString& output_dir) = 0;
    virtual QString get_version() = 0;
    QMap<QString, QString> get_config_files() {
        return config_files;
    }

protected:
    Configuration config;
    QMap<QString, QString> config_files;
};

#endif // COMPONENT_H
