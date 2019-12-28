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
    string input = "\
if is_screen_available({{screen_idx}}) then\n\
    os.execute(\"sudo Xephyr -softCursor -ac -br\n\
                  -mouse  'evdev,5,device=/dev/input/by-path/{{mouse_device}}'\n\
                  -keybd  'evdev,,device=/dev/input/by-path/{{keybd_device}}'\n\
                  -screen {{screen_width}}x{{screen_height}} :{{screen_idx}}\n\
                  &\")\n\
end\n\
";

    string output;
    for (uint32_t idx = 0; idx < seats.size(); idx++)
    {
        output = replace_all(input, "{{screen_idx}}",
                             to_string(idx + 1));
        output = replace_all(output, "{{mouse_device}}", seats[idx].mouse);
        output = replace_all(output, "{{keybd_device}}", seats[idx].keyboard);
        output = replace_all(output, "{{screen_width}}",
                             to_string(seats[idx].resolution.width));
        output = replace_all(output, "{{screen_height}}",
                             to_string(seats[idx].resolution.height));
        result << output;
    }
    return result.str();
}


