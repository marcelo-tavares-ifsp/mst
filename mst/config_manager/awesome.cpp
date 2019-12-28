#include "awesome.h"
#include "../common/utilites/utilites.h"

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
    string input = "\
if is_screen_available({{screen_idx}}) then\n\
    table.insert(\n\
        awful.rules.rules,\n\
        { rule = { class = \"Xephyr\",\n\
                   name  = \"Xephyr on :{{screen_idx}}.0 (ctrl+shift grabs mouse and keyboard)\" },\n\
          properties = { floating   = true,\n\
                         fullscreen = true,\n\
                         screen     = {{screen_idx}}} })\n\
end\n\
";

    for (uint32_t idx = 1; idx <= sSize; idx++)
    {
        result << replace_all(input, "{{screen_idx}}", to_string(idx));
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

    for (uint32_t idx = 0; idx < seats.size(); idx++)
    {
        string mouse_dev = "/dev/input/by-path/" + seats[idx].mouse;
        string keybd_dev = "/dev/input/by-path/" + seats[idx].keyboard;
        result << "if is_screen_available(" << (idx + 1) << ") then" << endl
               << "    os.execute(\"sudo Xephyr -softCursor -ac -br "
               << "        -mouse \'evdev,5,device=" << mouse_dev << "\'"
               << "        -keybd \'evdev,,device=" << keybd_dev << "\'"
               << "        -screen "
               << seats[idx].resolution.width  << "x"
               << seats[idx].resolution.height << " :"
               << idx + 1 << " &\")" << endl
               << "end" << endl;
    }
    return result.str();
}


