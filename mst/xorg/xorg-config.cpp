#include "xorg-config.h"



XorgConfig::XorgConfig(vector<Seat> seats) : seats(seats)
{
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
    for (int idx = 0; idx < config.seats.size(); idx++)
    {
        os << _section("Monitor")
           << _elem("Identifier") << string("\"")
           << "monitor" << idx << string("\"\n");
        if (idx < (config.seats.size() - 1))
        {
            os << _elem("Option") << "\"LeftOf\"\t"
               << "\"monitor" << idx + 1 << "\"\n";
        }

        os << _end_section("Monitor");
    }
}

static void _print_device(ostream& os, const XorgConfig& config)
{
    os << _section("Device")
       << _elem("Identifier") << string("\"card0\"\n");

    for (int idx = 0; idx < config.seats.size(); idx++)
    {
        os << _elem("Option")
           << string("\"Monitor-") << config.seats[idx].interface << string("\"")
           << string(" \"monitor") << idx    << string("\"\n");
    }

    os << _end_section("Device");
}

static void _print_screen(ostream& os, const XorgConfig& config)
{
    int total_width = config.seats.size() * config.seats[0].width;

    static const int depth = 24;
    os << _section("Screen")
       << _elem("Identifier")   << string("\"screen0\"")  << endl
       << _elem("Device")       << string("\"card0\"")    << endl
       << _elem("Monitor")      << string("\"monitor0\"") << endl
       << _elem("DefaultDepth") << depth          << endl
       << _sub_section("Display")
       << _sub_elem("Depth")    << depth          << endl
       << _sub_elem("Virtual")  << total_width << " " << config.seats[0].height << endl
       << _end_sub_section("Display")
       << _end_section("Screen");
}

static void _print_layout(ostream& os, const XorgConfig& config)
{
    for (int idx = 0; idx < config.seats.size(); idx++)
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
