#ifndef COMPONENT_H
#define COMPONENT_H

#include <QMap>
#include <QString>

#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"

class Component_error : public runtime_error {
public:
    Component_error(string what)
        : runtime_error(what) {
        // Do nothing.
    }
};

/**
 * @brief The Component_configuration class -- describes a MST component
 *     configuration.
 */
class Component_configuration
{
    friend class Component;

public:
    const QMap<QString, QString>& get_installation_paths() {
        return installation_paths;
    }

    void store(const QString& output_directory) {
        foreach (auto key, templates.keys()) {
            templates[key].substitute((output_directory
                                       + "/" + key).toStdString());
        }
    }

    void add(const QString& file_name, const QString& installation_path,
             const Template& file_template) {
        installation_paths[file_name] = installation_path;
        templates[file_name]          = file_template;
    }

    QMap<QString, Template> get_templates() {
        return templates;
    }

    Template& get_template(const QString& name) {
        return templates[name];
    }

private:
    QMap<QString, Template> templates;
    QMap<QString, QString>  installation_paths;
};

class Component
{
public:
    Component(Configuration& config);
    virtual void configure() = 0;
    virtual QString get_version() = 0;
    virtual void enable() = 0;
    virtual void disable() = 0;

    Component_configuration& get_configuration() {
        return component_configuration;
    }

protected:
    Configuration config;
    Component_configuration component_configuration;
};

#endif // COMPONENT_H
