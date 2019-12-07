#include "awesome.h"

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
    result << Awesome::make_xephyr_screens(seats)
           << "os.execute(\"unclutter &\")" << endl
    // TODO: 10s waiting seems to be enough for our cases, but this code
    //       likely will lead to some problems in the future.
    //       The better solution might be to wait for Xephyr to start in some
    //       kind of loop.
           << "os.execute(\"sleep 10; "
           << "sudo /usr/local/bin/mst-start-dm \""
           << ".. screen.count() .. \" &\")"
           << endl;

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
    for (uint32_t idx = 1; idx <= sSize; idx++)
    {
        result << "if is_screen_available(" << idx << ") then\n"
               << "    table.insert(awful.rules.rules, "
               << "        { rule = { class = \"Xephyr\", " << endl
               << "                   name = \"Xephyr on :" << idx << ".0 "
               << "(ctrl+shift grabs mouse and keyboard)\" }, "  << endl
               << "          properties = { floating = true, "   << endl
               << "                         fullscreen = true, " << endl
               << "                         screen = " << idx << "} })"
               << endl;
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


