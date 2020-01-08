#ifndef COMPONENT_H
#define COMPONENT_H

#include <QMap>
#include <QString>

#include "../configuration/configuration.h"

class Component_error : public runtime_error {
public:
    Component_error(string what)
        : runtime_error(what) {
        // Do nothing.
    }
};

class Component
{
public:
    Component(Configuration& config);
    virtual void configure(const QString& output_dir) = 0;
    virtual QString get_version() = 0;
    virtual void enable() = 0;
    virtual void disable() = 0;

    QMap<QString, QString> get_config_files() {
        return config_files;
    }

protected:
    Configuration config;
    QMap<QString, QString> config_files;
};

#endif // COMPONENT_H
