#include <iostream>

#include "xorg-config.h"
#include "xorg-monitor.h"

using namespace std;

XorgConfig::XorgConfig()
{

}

void XorgConfig::add_monitor(XorgMonitor device)
{
    monitors.push_back(device);
}

const vector<XorgMonitor>& XorgConfig::get_monitors() const
{
    return monitors;
}


/* Config elements' constructors. */

static const string _section(const string& name)
{
    return "Section: \"" + name + "\"\n";
}

static const string _sub_section(const string& name)
{
    return "        SubSection: \"" + name + "\"\n";
}

static const string _end_section(const string& name)
{
    return "EndSection\t# " + name + "\n\n";
}

static const string _end_sub_section(const string& name)
{
    return "    EndSubSection\t# " + name + "\n\n";
}

static const string _elem(const string& name)
{
   return "    " + name + "\t\t";
}

static const string _sub_elem(const string& name)
{
    return "    " + _elem(name);
}


/* Config printers. */

static void _print_monitors(ostream& os, const XorgConfig& config)
{
    for (auto const& monitor : config.get_monitors())
    {
        os << _section("Monitor")
           << _elem("Identifier") << string("\"")
           << monitor.get_identifier() << string("\"\n")
           << _end_section("Monitor");
    }
}

static void _print_device(ostream& os, const XorgConfig& config)
{
    os << _section("Device")
       << _elem("Identifier") << string("\"card0\"\n");

    for (auto const& monitor : config.get_monitors())
    {
        os << _elem("Option")
           << string("\"") << monitor.get_interface_name() << string("\"")
           << string("\"") << monitor.get_identifier()     << string("\"\n");
    }

    os << _end_section("Device");
}

static void _print_screen(ostream& os, const XorgConfig& config)
{
    static const int depth = 24;
    os << _section("Screen")
       << _elem("Identifier")   << string("\"screen0\"")  << endl
       << _elem("Device")       << string("\"card0\"")    << endl
       << _elem("Monitor")      << string("\"monitor0\"") << endl
       << _elem("DefaultDepth") << depth          << endl
       << _sub_section("Display")
       << _sub_elem("Depth")    << depth          << endl
       << _sub_elem("Virtual")  << 3200 << " " << 1080 << endl
       << _end_sub_section("Display")
       << _end_section("Screen");
}

ostream& operator << (ostream& os, const XorgConfig& config)
{
    _print_monitors(os, config);
    _print_device(os, config);
    _print_screen(os, config);
    return os;
}
