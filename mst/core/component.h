/* component.h -- MST component class.
 *
 * Copyright (C) 2018-2022 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2018-2022 Artyom V. Poptsov <a@gkaz.ru>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef COMPONENT_H
#define COMPONENT_H

#include <QMap>
#include <QString>

#include "configuration.h"
#include "types/template.h"
#include "template_manager.h"
#include "platform.h"

Q_DECLARE_LOGGING_CATEGORY(component_category)

/**
 * @brief The Component_error class -- describes a generic component error.
 */
class Component_error : public std::runtime_error {
public:
    Component_error(std::string what)
        : std::runtime_error(what) {
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
    Component_configuration(Configuration& config);

    /**
     * @brief get_installation_paths -- Get installation paths
     *      for the component.
     * @return A hash map where key is file name and value is the
     *      installation path for the file.
     */
    const QMap<QString, QString>& get_installation_paths() {
        return installation_paths;
    }

    /**
     * @brief get_installation_path -- Get installation path
     *      for the specified file.
     * @param file_name
     * @return Installation path as a string.
     */
    const QString get_installation_path(const QString& file_name) const;

    /**
     * @brief store -- Store confituration to the specified output directory.
     * @param output_directory -- A path to the output directory.
     */
    void store(const QString& output_directory);

    /**
     * @brief backup -- Create a backup copy of the component onfiguration.
     * @param backup_directory -- A directory to store backups.
     */
    void backup(const QString& backup_directory);

    /**
     * @brief restore -- Restore a backup copy of the configuration.
     * @param backup_directory -- A directory to store backups.
     */
    void restore(const QString& backup_directory);

    /**
     * @brief add -- Add a new configuration file to the configuration.
     * @param file_name -- A name of configuration file.
     * @param installation_path -- An installation path for the configuration
     *     file.
     * @param file_template -- A template for the configuration file.
     */
    void add(const QString& file_name, const QString& installation_path,
             const Template& file_template);

    /**
     * @brief get_templates -- Get templates for configuration files.
     * @return A QMap with file name as a key and template as a value.
     */
    QMap<QString, Template> get_templates() {
        return templates;
    }

    /**
     * @brief get_template -- Get a configuration template by its file name.
     * @param name -- A configuration file name.
     * @return -- A template.
     */
    Template& get_template(const QString& name) {
        return templates[name];
    }

private:
    Configuration& config;

    /**
     * @brief templates -- Configuration templates.
     */
    QMap<QString, Template> templates;
    /**
     * @brief installation_paths -- Installation paths for configuration files.
     */
    QMap<QString, QString>  installation_paths;
};

/**
 * @brief The Component class -- describes an MST component that can be
 *     configured, installed and de-installed.
 */
class Component
{
public:
    Component(const QString& name, Configuration& config);

    const QString& get_name() const;

    /**
     * @brief configure -- Configure the comonent.
     */
    virtual void configure() = 0;
    /**
     * @brief get_version -- Get the component version.
     * @return A version string.
     */
    virtual QString get_version() {
        return "(version is unknown)";
    }

    /**
     * @brief enable -- Enable the component.
     */
    virtual void enable() {
        /* Do nothing. */
    }

    /**
     * @brief disable -- Disable the component.
     */
    virtual void disable() {
        /* Do nothing. */
    }

    /**
     * @brief install -- Install the component to the system.
     */
    virtual void install();

    /**
     * @brief start -- Start the component.
     */
    virtual void start() {
        /* Do nothing. */
    }

    /**
     * @brief stop -- Stop the component without disabling it so
     *      it can be started again (e.g. after reboot.)
     */
    virtual void stop() {
        /* Do nothing. */
    }

    /**
     * @brief get_configuration -- Returns a component configuration.
     * @return A component configuration.
     */
    Component_configuration& get_configuration() {
        return component_configuration;
    }

protected:
    /**
     * @brief config -- An MST configuration.
     */
    Configuration& config;

    /**
     * @brief component_configuration -- A configuration of this component.
     */
    Component_configuration component_configuration;

private:
    QString name = nullptr;

    /**
     * @brief install -- Install a file to the specified destination.
     * @param src -- Source file.
     * @param dst -- Destination.
     */
    void install(const QString& src, const QString& dst);
};

#endif // COMPONENT_H
