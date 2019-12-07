#include "configmanager.h"

Q_LOGGING_CATEGORY(config_manager_category, "mst.config_manager")

/**
 * @brief _make_xephyr_screens -- Generate Awesome "rc.lua" code that starts
 *     Xephyr instances.
 * @param seats -- Number of seats.
 * @return Generated Lua code as a string.
 */
static string _make_xephyr_screens(vector<Seat> seats)
{
    stringstream result;

    for (uint32_t idx = 0; idx < seats.size(); idx++)
    {
        string mouse_dev = "/dev/input/by-path/" + seats[idx].mouse;
        string keybd_dev = "/dev/input/by-path/" + seats[idx].keyboard;
        result << "os.execute(\"sudo Xephyr -softCursor -ac -br "
               << " -mouse \'evdev,5,device=" << mouse_dev << "\'"
               << " -keybd \'evdev,,device=" << keybd_dev << "\'"
               << " -screen "
               << seats[idx].resolution.width  << "x"
               << seats[idx].resolution.height << " :"
               << idx + 1 << " &\")" << endl;
    }
    return result.str();
}

/**
 * @brief make_xephyr_rules -- Generate Awesome rules to arrange Xephyr
 *     instances on the screens.
 * @param sSize -- Number of seats.
 * @return Generated Lua code as a string.
 */
static string make_xephyr_rules(uint32_t sSize)
{
    string rules;
    for (uint32_t idx = 1; idx <= sSize; idx++)
    {
        rules += string("{ rule = { class = \"Xephyr\", name = \"Xephyr on :");
        rules += to_string(idx);
        rules += string(".0 (ctrl+shift grabs mouse and keyboard)\" },\n ");
        rules += string("properties = { floating = true, fullscreen = true, screen = ");
        rules += to_string(idx);
        rules += string("} },\n");
    }
    return rules;
}

/**
 * @param seats
 * @brief make_xephyr_autostart -- Generate Xephyr autostart commands for
 *     "rc.lua".
 *
 * Generate Lua code that starts Xephyr instances from Awesome "rc.lua" file.
 *
 * @param seats -- Number of seats.  This parameter affects the number of Xephyr
 *     instances.
 * @return Generated Lua code as a string.
 */
static string make_xephyr_autostart(vector<Seat> seats)
{
    string result;
    result += _make_xephyr_screens(seats);
    result += "os.execute(\"unclutter &\")\n";

    // TODO: 10s waiting seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop.
    result += (string) "os.execute(\"sleep 10; "
            + "sudo /usr/local/bin/mst-start-dm "
            + to_string(seats.size()) + " &\")\n";

    return result;
}

/**
 * @brief ConfigManager::make_rc_lua -- Generate Awesome WM "rc.lua" file.
 *
 * Generate Awesome WM "rc.lua" file based on a template.
 *
 * @param config
 */
void ConfigManager::make_rc_lua(Configuration& config)
{
    string out_file = PathManager::get_instance()->get_rclua_config();
    fstream rclua_pattern;
    fstream rclua;

    const vector<int> version = CommandManager::get_awesome_version();
    if (version[0] == 3)
    {
        qDebug(config_manager_category) << "Using rc.lua.template for Awesome 3";
        rclua_pattern.open(PathManager::get_instance()->get_rclua_template(), ios::in);
    }
    else
    {
        qDebug(config_manager_category) << "Using rc.lua.template for Awesome 4";
        rclua_pattern.open(PathManager::get_instance()->get_rclua4_template(), ios::in);
    }
    rclua.open(out_file, ios::out);
    if (! rclua_pattern.is_open()) {
        //throw "ERROR";
    }
    string str;

    qInfo(config_manager_category) << "writing '" << out_file.c_str() << "' ...";

    while(getline(rclua_pattern, str))
    {
        str = replace_all(str, "{{mst_autostart}}", make_xephyr_autostart(config.seats));
        str = replace_all(str, "{{mst_awful_rules}}", make_xephyr_rules(config.seats.size()));

        rclua << str << endl;
    }

    rclua.close();
    rclua_pattern.close();
    qDebug(config_manager_category) << "writing '" << out_file.c_str() << "' ... done";
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
    string out_file = PathManager::get_instance()->get_bashrc_config();
    string in_file = PathManager::get_instance()->get_bashrc_config_template();
    ofstream bashrc(out_file);
    ifstream bashrc_template(in_file);

    for (string line; getline(bashrc_template, line); )
    {
        string tmp = replace_all(line, "{{tty}}", "1");
        bashrc << tmp << endl;
    }

    bashrc.close();
    bashrc_template.close();
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
    const string out_file = PathManager::get_instance()->get_sudoers_config();
    const string in_file = PathManager::get_instance()->get_sudoers_config_template();
    ofstream out(out_file);
    ifstream in(in_file);

    string line;
    getline(in, line);
    string result = replace_all(replace_all(line, "{{user}}", user),
                                "{{mst}}",
                                "/usr/local/bin/mst-start-dm");
    out << result << endl;
    out.close();
    in.close();
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
    const string out_file = PathManager::get_instance()->get_getty_service_config();
    const string in_file = PathManager::get_instance()->get_getty_service_config_template();
    const string user = PathManager::get_instance()->get_mst_user();
    ifstream in(in_file);
    ofstream out(out_file);

    for (string line; getline(in, line); )
    {
        string tmp = replace_all(line, "{{user}}", user);
        out << tmp << endl;
    }
    in.close();
    out.close();
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
    const string out_file = PathManager::get_instance()->get_vgl_config();
    const string in_file = PathManager::get_instance()->get_vgl_config_template();
    const string user = PathManager::get_instance()->get_mst_user();
    ofstream out(out_file);
    ifstream in(in_file);

    for (string line; getline(in, line); )
    {
        string tmp = replace_all(line, "{{user}}", user);
        out << tmp << endl;
    }

    out.close();
    in.close();
}
