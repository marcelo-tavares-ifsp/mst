#include "awesome.h"
#include "../common/utilites/utilites.h"

#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"

using namespace std;

Awesome::Awesome()
{

}

/**
 * @brief make_xephyr_autostart -- Generate Xephyr autostart commands for
 *     "rc.lua".
 *
 * Generate Lua code that starts Xephyr instances from Awesome "rc.lua" file.
 *
 * @return Generated Lua code as a string.
 */
string Awesome::make_xephyr_autostart()
{
    // TODO: 10s sleep seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop (see the file.)
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/mst_autostart.lua");

    return tpl.substitute();
}

/**
 * @brief make_xephyr_rules -- Generate Awesome rules to arrange Xephyr
 *     instances on the screens.
 * @param sSize -- Number of seats.  This parameter affects the number of Xephyr
 *     instances.
 * @return Generated Lua code as a string.
 */
string Awesome::make_xephyr_rules(uint32_t sSize)
{
    stringstream result;
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/xephyr_rules.lua");

    for (uint32_t idx = 1; idx <= sSize; idx++)
    {
        result << tpl.set("screen_idx", to_string(idx)).substitute();
    }
    return result.str();
}

/**
 * @brief _make_xephyr_screens -- Generate Awesome "rc.lua" code that starts
 *     Xephyr instances.
 * @param seats -- Number of seats.
 * @return Generated Lua code as a string.
 */
string Awesome::make_xephyr_screens(vector<Seat> seats)
{
    stringstream result;
    Template tpl = Template_manager::get_instance()
            ->get_template("awesome/xephyr_screens.lua");

    for (uint32_t idx = 0; idx < seats.size(); idx++)
    {
        tpl.set("screen_idx",    to_string(idx + 1));
        tpl.set("mouse_device",  seats[idx].mouse);
        tpl.set("keybd_device",  seats[idx].keyboard);
        tpl.set("screen_width",  to_string(seats[idx].resolution.width));
        tpl.set("screen_height", to_string(seats[idx].resolution.height));

        result << tpl.substitute();
    }
    return result.str();
}


