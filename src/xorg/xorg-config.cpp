#include "xorg-config.h"



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
    return "Section \"" + name + "\"\n";
}

static const string _sub_section(const string& name)
{
    return "    SubSection \"" + name + "\"\n";
}

static const string _end_section(const string& name)
{
    return "EndSection\t# " + name + "\n\n";
}

static const string _end_sub_section(const string& name)
{
    return "    EndSubSection\t# " + name + "\n";
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
    vector<XorgMonitor> m = config.get_monitors();
    uint16_t total_width = accumulate(m.begin(), m.end(),
                                      0,
                                      [](uint16_t sz, XorgMonitor& m) {
       return sz + m.get_width();
    });

    uint16_t min_height = accumulate(m.begin(), m.end(), m[0].get_height(),
                                     [](uint16_t min, XorgMonitor& m) {
        return min > m.get_height() ? m.get_height() : min;
    });

    static const int depth = 24;
    os << _section("Screen")
       << _elem("Identifier")   << string("\"screen0\"")  << endl
       << _elem("Device")       << string("\"card0\"")    << endl
       << _elem("Monitor")      << string("\"monitor0\"") << endl
       << _elem("DefaultDepth") << depth          << endl
       << _sub_section("Display")
       << _sub_elem("Depth")    << depth          << endl
       << _sub_elem("Virtual")  << total_width << " " << min_height << endl
       << _end_sub_section("Display")
       << _end_section("Screen");
}

static void _print_layout(ostream& os, const XorgConfig& config)
{
    int size = config.get_monitors().size();
    for (int idx = 0; idx < size; idx++)
    {
        os << _section("ServerLayout")
           << _elem("Identifier") << string("\"seat")       << idx    << "\"\n"
           << _elem("Screen")     << string("\"screen0\"\n")
           << _elem("Option")     << string("\"Seat\"\t")   << string("\"seat")
           << idx << "\"\n"
           << _end_section("ServerLayout");
    }
}

ostream& operator << (ostream& os, const XorgConfig& config)
{
    _print_monitors(os, config);
    _print_device(os, config);
    _print_screen(os, config);
    _print_layout(os, config);
    return os;
}
