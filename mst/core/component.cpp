#include "component.h"
#include "platform.h"

Component::Component(Configuration& config)
    : config(config),
      component_configuration(Component_configuration(config))
{
    /* Do nothing */
}

Component_configuration::Component_configuration(Configuration &config)
    : config(config)
{

}

void Component_configuration::store(const QString &output_directory) {
    foreach (auto key, templates.keys()) {
        templates[key].substitute(output_directory + "/" + key);
    }
}

void Component_configuration::backup(const QString &backup_directory) {
    const QString mst_user = config.get_system_mst_user();
    const QString mst_user_home = "/home/" + mst_user + "/";
    foreach (auto key, installation_paths.keys()) {
        QString dest = installation_paths[key];
        if (dest.contains("{{home}}")) {
            Template tpl(dest);
            tpl.set("home", mst_user_home);
            dest = tpl.substitute();
        }
        try {
            Platform::fs_cp(dest, backup_directory + "/" + key);
        } catch (Platform_exception& e) {
            //qWarning(install_controller_category) << e.what();
        }
    }
}

void Component_configuration::restore(const QString &backup_directory) {
    const QString mst_user = config.get_system_mst_user();
    const QString mst_user_home = "/home/" + mst_user + "/";
    foreach (auto key, installation_paths.keys()) {
        QString dest = installation_paths[key];
        if (dest.contains("{{home}}")) {
            Template tpl(dest);
            tpl.set("home", mst_user_home);
            dest = tpl.substitute();
        }
        Platform::fs_cp(backup_directory + "/" + key, dest);
    }
}

void Component_configuration::add(const QString &file_name, const QString &installation_path, const Template &file_template) {
    installation_paths[file_name] = installation_path;
    templates[file_name]          = file_template;
}
