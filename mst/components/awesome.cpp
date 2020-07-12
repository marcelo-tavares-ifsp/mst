#include <memory>

#include "awesome.h"
#include "component.h"
#include "../core/utilites/utilites.h"
#include "../core/types/monitor.h"

#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"
#include "../path_manager/pathmanager.h"

using namespace std;
using namespace awesome;

Q_LOGGING_CATEGORY(component_awesome_category, "mst.component.awesome")

Awesome::Awesome(Configuration& config)
    : Component(config)
{
    /* Do nothing. */
}

void Awesome::configure()
{
    QString template_name;

    const vector<int> version = get_awesome_version();
    if (version[0] == 3)
    {
        qDebug(component_awesome_category)
                << "Using rc.lua.template for Awesome 3";
        template_name = RC_LUA_TPL_FILE;
    }
    else
    {
        qDebug(component_awesome_category)
                << "Using rc.lua.template for Awesome 4";
        template_name = RC_LUA4_TPL_FILE;
    }

    Template rclua_template
            = Template_manager::get_instance()->get_template(template_name);
    prepare_rclua_template(rclua_template);
    component_configuration.add(RC_LUA_FILE, "{{home}}/.config/awesome/rc.lua",
                                rclua_template);
}

QString Awesome::get_version()
{
    return QString::fromStdString(get_awesome_raw_version());
}

void Awesome::prepare_rclua_template(Template& rclua_template)
{
    rclua_template
        .set("mst_autostart",   make_xephyr_autostart())
        .set("xephyr_screens",  make_xephyr_screens(config.seats))
        .set("mst_awful_rules", make_xephyr_rules(config.seats.size()));
}

//// Helper procedures.

/**
 * @brief make_xephyr_autostart -- Generate Xephyr autostart commands for
 *     "rc.lua".
 *
 * Generate Lua code that starts Xephyr instances from Awesome "rc.lua" file.
 *
 * @return Generated Lua code as a string.
 */
QString awesome::make_xephyr_autostart()
{
    // TODO: 10s sleep seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop (see the file.)
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/mst_autostart.lua");
    tpl.set("prefix", QString::fromLocal8Bit(INSTALLATION_PREFIX));
    return tpl.substitute();
}

/**
 * @brief make_xephyr_rules -- Generate Awesome rules to arrange Xephyr
 *     instances on the screens.
 * @param sSize -- Number of seats.  This parameter affects the number of Xephyr
 *     instances.
 * @return Generated Lua code as a string.
 */
QString awesome::make_xephyr_rules(uint32_t sSize)
{
    QString result = "";
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/xephyr_rules.lua");

    for (uint32_t idx = 1; idx <= sSize; idx++)
    {
        result += tpl.set("screen_idx", QString::number(idx)).substitute();
    }
    return result;
}

/**
 * @brief _make_xephyr_screens -- Generate Awesome "rc.lua" code that starts
 *     Xephyr instances.
 * @param seats -- Number of seats.
 * @return Generated Lua code as a string.
 */
QString awesome::make_xephyr_screens(vector<shared_ptr<Seat>> seats)
{
    QString result = "";
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/xephyr_screens.lua");

    for (uint32_t idx = 0; idx < seats.size(); idx++)
    {
        tpl.set("screen_idx",    QString::number(idx + 1));
        tpl.set("mouse_device",  seats[idx]->get_mouse());
        tpl.set("keybd_device",  seats[idx]->get_keyboard());
        tpl.set("screen_width",  QString::number(seats[idx]->get_monitor().get_current_resolution().get_width()));
        tpl.set("screen_height", QString::number(seats[idx]->get_monitor().get_current_resolution().get_height()));

        result += tpl.substitute();
    }
    return result;
}

/**
 * @brief get_awesome_version -- get Awesome version.
 * @throws an error message on error.
 * @return awesome version as a string.
 */
string awesome::get_awesome_raw_version()
{
    const int BUF_SZ = 255;
    char buf[BUF_SZ];
    FILE* f = popen("awesome --version", "r");
    if (f != NULL)
    {
        if (fgets(buf, BUF_SZ, f) != NULL)
        {
            return string(buf);
        }
    }
    else
    {
        const string msg = "Could not get Awesome version.";
        qCritical(component_awesome_category) << msg.c_str();
        throw msg;
    }
    return NULL;
}

vector<int> awesome::get_awesome_version()
{
    const string raw_version = get_awesome_raw_version();
    regex r1("awesome v([0-9]+).([0-9]+)*");
    smatch sm;
    if (regex_search(raw_version, sm, r1))
    {
        vector<int> result(2);
        result[0] = atoi(string(sm[1]).c_str());
        result[1] = atoi(string(sm[2]).c_str());
        return result;
    }
    else
    {
        const string msg = "Could not parse Awesome version: " + raw_version;
        qCritical(component_awesome_category) << msg.c_str();
        throw msg;
    }
}
