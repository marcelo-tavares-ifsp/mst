#include "awesome-config.h"
#include "awesome-device.h"

#include <vector>
#include <string>
#include <iostream>

using namespace std;

AwesomeConfig::AwesomeConfig()
{

}

void AwesomeConfig::add_devices(AwesomeDevice pair_devices)
{
    devices.push_back(pair_devices);
}

const vector<AwesomeDevice>& AwesomeConfig::get_devices() const
{
    return devices;
}

static void _print_xephyr(ostream& os, const AwesomeConfig& config)
{
    for (auto const& pair_devices : config.get_devices())
    {
        os << string("os.execute(\"Xephyr -softCursor -ac -br -mouse \'")
           << pair_devices.get_mouse()      << string("\' -keybd \'")
           << pair_devices.get_keyboard()   << string("\' -screen ")
           << pair_devices.get_width()      << string("x")
           << pair_devices.get_height()     << string(" :")
           << pair_devices.get_identifier() << string(" &\")")   << endl;
    }
}

static void _print_unclutter(ostream& os, const AwesomeConfig& config)
{
    os << string("os.execute(\"unclutter &\")") << endl;
}

static void _print_script(ostream& os, const AwesomeConfig& config)
{
    os << string("os.execute(\"sleep 5; sudo /root/src/mst/mst 2 &\")") << endl;
}

string AwesomeConfig::get_rules()
{
    string rules;
    for (auto const& pair_devices : devices)
    {
        rules += string("{ rule = { class = \"Xephyr\", name = \"Xephyr on :");
        rules += pair_devices.get_identifier();
        rules += string(".0 (ctrl+shift grabs mouse and keyboard)\" },\n ");
        rules += string("properties = { floating = true, fullscreen = true, screen = ");
        rules += pair_devices.get_identifier();
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
