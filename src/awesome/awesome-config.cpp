#include "awesome-config.h"



AwesomeConfig::AwesomeConfig(vector<Seat> seats) : seats(seats)
{
}

static void _print_xephyr(ostream& os, const AwesomeConfig& config)
{
    for (int idx = 0; idx < config.seats.size(); idx++)
    {
        string mouse_dev = "/dev/input/by-path/" + config.seats[idx].mouse;
        string keybd_dev = "/dev/input/by-path/" + config.seats[idx].keyboard;
        os << string("os.execute(\"Xephyr -softCursor -ac -br -mouse \'")
           << mouse_dev << string("\' -keybd \'")
           << keybd_dev << string("\' -screen ")
           << config.seats[idx].width      << string("x")
           << config.seats[idx].height     << string(" :")
           << idx                          << string(" &\")")   << endl;
    }
}

static void _print_unclutter(ostream& os, const AwesomeConfig& config)
{
    os << string("os.execute(\"unclutter &\")") << endl;
}

static void _print_script(ostream& os, const AwesomeConfig& config)
{
    os << string("os.execute(\"sleep 5; sudo /root/src/mst/mst ")
       << config.seats.size() << " &\")" << endl;
}

string AwesomeConfig::get_rules()
{
    string rules;
    for (int idx = 0; idx < seats.size(); idx++)
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

ostream& operator << (ostream& os, const AwesomeConfig& config)
{
    _print_xephyr(os, config);
    _print_unclutter(os, config);
    _print_script(os, config);
    return os;
}
