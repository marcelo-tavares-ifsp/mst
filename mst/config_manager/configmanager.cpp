#include "configmanager.h"

#include "template_manager/template_manager.h"
#include "template_manager/template.h"

#include "vgl.h"

Q_LOGGING_CATEGORY(config_manager_category, "mst.config_manager")

/**
 * @brief ConfigManager::make_rc_lua -- Generate Awesome WM "rc.lua" file.
 *
 * Generate Awesome WM "rc.lua" file based on a template.
 *
 * @param config
 */
void ConfigManager::make_rc_lua(Configuration& config)
{
    awesome::Awesome awesome(config);
    awesome.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

/**
 * @brief ConfigManager::make_xorg -- Make Xorg configuration file based on a
 *     template.
 * @param config -- MST configuration.
 */
void ConfigManager::make_xorg(Configuration& config)
{
    xorg::Xorg xorg(config);
    xorg.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));
}

/**
 * @brief ConfigManager::make_bashrc -- Generate ".bashrc" file for multiseat
 *     user.
 */
void ConfigManager::make_bashrc()
{
    string out_file_name = PathManager::get_instance()->get_bashrc_config();
    string template_name
            = PathManager::get_instance()->get_bashrc_config_template();
    Template bashrc_template
            = Template_manager::get_instance()->get_template(template_name);
    bashrc_template.set("tty", "1").substitute(out_file_name);
}

/**
 * @brief ConfigManager::make_xinitrc -- Generate ".xinitrc" file.
 */
void ConfigManager::make_xinitrc()
{
    string out_file_name
            = PathManager::get_instance()->get_xinitrc_config();
    string tpl_name
            = PathManager::get_instance()->get_xinitrc_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);
    tpl.substitute(out_file_name);

    out_file_name = PathManager::get_instance()->get_xmst_config();
    tpl_name  = PathManager::get_instance()->get_xmst_config_template();
    tpl = Template_manager::get_instance()->get_template(tpl_name);
    tpl.substitute(out_file_name);
}

/**
 * @brief ConfigManager::make_sudoers -- Generate sudoers file for MST.
 */
void ConfigManager::make_sudoers()
{
    const string user = PathManager::get_instance()->get_mst_user();
    const string out_file_name
            = PathManager::get_instance()->get_sudoers_config();
    const string tpl_name
            = PathManager::get_instance()->get_sudoers_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);

    tpl.set("user", user).set("mst", "/usr/local/bin/mst-start-dm")
            .substitute(out_file_name);
}

/**
 * @brief Controller::make_lightdm_conf -- Generate a LightDM configuration
 *          file.
 *
 * TODO: Use actual number of seats.
 */
void ConfigManager::make_lightdm_conf()
{
    const string out_file_name
            = PathManager::get_instance()->get_lightdm_mst_config();
    const string tpl_name
            = PathManager::get_instance()->get_lightdm_mst_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);
    tpl.substitute(out_file_name);
}

void ConfigManager::make_getty_service()
{
    const string out_file_name
            = PathManager::get_instance()->get_getty_service_config();
    const string tpl_name
            = PathManager::get_instance()->get_getty_service_config_template();
    const string user = PathManager::get_instance()->get_mst_user();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);
    tpl.set("user", user).substitute(out_file_name);
}

/**
 * @brief ConfigManager::make_udev_rules -- Generate udev rules for MST.
 * @param seats -- A vector of MST seats.
 */
void ConfigManager::make_udev_rules(vector<Seat> seats)
{
    const string out_file = PathManager::get_instance()->get_udev_rules_config();
    Template tpl = Template_manager::get_instance()
            ->get_template("99-mst.rules");
    ofstream out(out_file);

    for (uint32_t idx = 0; idx < seats.size(); ++idx)
    {
        out << tpl.set("usb_device", seats[idx].usb)
               .set("seat_idx", to_string(idx + 1 ))
               .substitute();
    }

    out.close();
}

void ConfigManager::make_udev_service()
{
    const string out_file_name
            = PathManager::get_instance()->get_systemd_udev_config();
    const string tpl_name
            = PathManager::get_instance()->get_systemd_udev_config_template();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);
    tpl.substitute(out_file_name);
}

void ConfigManager::make_vgl(Configuration& config)
{
    vgl::VGL vgl(config);
    vgl.configure(
                QString::fromStdString(
                    PathManager::get_instance()->get_output_dir()));

}
