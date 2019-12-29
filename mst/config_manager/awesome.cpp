#include "awesome.h"
#include "../common/utilites/utilites.h"

#include "../template_manager/template.h"

using namespace std;

Awesome::Awesome()
{

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
string Awesome::make_xephyr_autostart(vector<Seat> seats)
{
    stringstream result;
    // TODO: 10s sleep seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop.
    string execute_commands = "\
os.execute(\"unclutter &\")\n\
os.execute(\"sleep 10; sudo /usr/local/bin/mst-start-dm \" .. screen.count() .. \" &\")\n\
";
    result << Awesome::make_xephyr_screens(seats)
           << execute_commands;

    return result.str();
}

/**
 * @brief make_xephyr_rules -- Generate Awesome rules to arrange Xephyr
 *     instances on the screens.
 * @param sSize -- Number of seats.
 * @return Generated Lua code as a string.
 */
string Awesome::make_xephyr_rules(uint32_t sSize)
{
    stringstream result;
    Template tpl("\
if is_screen_available({{screen_idx}}) then\n\
    table.insert(\n\
        awful.rules.rules,\n\
        { rule = { class = \"Xephyr\",\n\
                   name  = \"Xephyr on :{{screen_idx}}.0 (ctrl+shift grabs mouse and keyboard)\" },\n\
          properties = { floating   = true,\n\
                         fullscreen = true,\n\
                         screen     = {{screen_idx}}} })\n\
end\n\
");

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
    Template tpl("\
if is_screen_available({{screen_idx}}) then\n\
    os.execute(\"sudo Xephyr -softCursor -ac -br\n\
                  -mouse  'evdev,5,device=/dev/input/by-path/{{mouse_device}}'\n\
                  -keybd  'evdev,,device=/dev/input/by-path/{{keybd_device}}'\n\
                  -screen {{screen_width}}x{{screen_height}} :{{screen_idx}}\n\
                  &\")\n\
end\n\
");

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


