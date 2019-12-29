#include "configmanager.h"

#include "template_manager/template_manager.h"
#include "template_manager/template.h"

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
    string out_file_name = PathManager::get_instance()->get_rclua_config();
    fstream rclua_pattern;
    fstream rclua;
    string template_name;

    const vector<int> version = CommandManager::get_awesome_version();
    if (version[0] == 3)
    {
        qDebug(config_manager_category) << "Using rc.lua.template for Awesome 3";
        template_name = PathManager::get_instance()->get_rclua_template();
    }
    else
    {
        qDebug(config_manager_category) << "Using rc.lua.template for Awesome 4";
        template_name = PathManager::get_instance()->get_rclua4_template();
    }

    qInfo(config_manager_category) << "writing '" << out_file_name.c_str()
                                   << "' ...";

    Template rclua_template
            = Template_manager::get_instance()->get_template(template_name);
    rclua_template
            .set("mst_autostart",
                  Awesome::make_xephyr_autostart(config.seats))
            .set("mst_awful_rules",
                  Awesome::make_xephyr_rules(config.seats.size()))
            .substitute(out_file_name);

    qDebug(config_manager_category) << "writing '" << out_file_name.c_str()
                                    << "' ... done";
}

/**
 * @brief ConfigManager::make_xorg -- Make Xorg configuration file based on a
 *     template.
 * @param config -- MST configuration.
 */
void ConfigManager::make_xorg(Configuration& config)
{
    string out_file = PathManager::get_instance()->get_xorg_config();
    XorgConfig* xorg_conf = new XorgConfig(config.seats);

    fstream xorg;
    xorg.open(out_file, ios::out);
    xorg << *xorg_conf;
    xorg.close();
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
    string out_file = PathManager::get_instance()->get_xinitrc_config();
    string in_file = PathManager::get_instance()->get_xinitrc_config_template();
    ofstream xinitrc(out_file, ios::binary);
    ifstream xinitrc_template(in_file, ios::binary);

    xinitrc << xinitrc_template.rdbuf();
    xinitrc.close();
    xinitrc_template.close();

    out_file = PathManager::get_instance()->get_xmst_config();
    in_file  = PathManager::get_instance()->get_xmst_config_template();
    ofstream xmst(out_file);
    ifstream xmst_template(in_file);
    xmst << xmst_template.rdbuf();
    xmst.close();
    xmst_template.close();
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
    const string out_file = PathManager::get_instance()->get_lightdm_mst_config();
    const string in_file = PathManager::get_instance()->get_lightdm_mst_config_template();
    ofstream out(out_file);
    ifstream in(in_file);

    out << in.rdbuf();
    out.close();
    in.close();
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
    ofstream out(out_file);

    for (uint32_t idx = 0; idx < seats.size(); ++idx)
    {
        out << "ACTION==\"add\", ";
        out << "KERNEL==\"sd[b-z][0-9]\", ";
        out << "DEVPATH==\"" << seats[idx].usb << "/*\", ";
        out << "RUN+=\"/usr/local/bin/mst-mount /dev/%k " << idx + 1 << "\""
            << endl;
    }

    out.close();
}

void ConfigManager::make_udev_service()
{
    const string out_file = PathManager::get_instance()->get_systemd_udev_config();
    const string in_file = PathManager::get_instance()->get_systemd_udev_config_template();
    ofstream out(out_file);
    ifstream in(in_file);

    out << in.rdbuf();
    out.close();
    in.close();
}

void ConfigManager::make_vgl()
{
    const string out_file_name = PathManager::get_instance()->get_vgl_config();
    const string tpl_name
            = PathManager::get_instance()->get_vgl_config_template();
    const string user = PathManager::get_instance()->get_mst_user();
    Template tpl = Template_manager::get_instance()->get_template(tpl_name);
    tpl.set("user", user).substitute(out_file_name);
}
