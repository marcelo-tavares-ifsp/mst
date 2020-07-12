#include <sstream>

#include "xorg.h"
#include "../configuration.h"
#include "../types/template.h"
#include "core/template_manager.h"

using namespace xorg;

Xorg::Xorg(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

/* Config elements' constructors. */

static const string _section(const string& name)
{
    return "Section \"" + name + "\"\n";
}

static const string _end_section(const string& name)
{
    return "EndSection\t# " + name + "\n\n";
}

static const string _sub_section(const string& name)
{
    return "\tSubSection \"" + name + "\"\n";
}

static const string _end_sub_section(const string& name)
{
    return "\tEndSubSection\t# " + name + "\n";
}

static const string _elem(const string& name)
{
   return "\t" + name + "\t\t";
}

static const string _sub_elem(const string& name)
{
    return "\t" + _elem(name);
}


/* Config printers. */

static void _print_monitors(ostream& os, const Configuration& config)
{
    for (uint32_t idx = 0; idx < config.seats.size(); idx++)
    {
        os << _section("Monitor")
           << _elem("Identifier") << string("\"") << "monitor" << idx << string("\"\n");

        if (idx < (config.seats.size() - 1))
        {
            os << _elem("Option") << "\"LeftOf\"\t" << "\"monitor" << idx + 1 << "\"\n";
        }

        os << _end_section("Monitor");
    }
}

static void _print_device(ostream& os, const Configuration& config)
{
    os << _section("Device")
       << _elem("Identifier") << string("\"card0\"\n");

    for (uint32_t idx = 0; idx < config.seats.size(); idx++)
    {
        os << _elem("Option")
           << string("\"Monitor-") << config.seats[idx]->get_monitor().get_interface().toStdString()
           << string("\"")
           << string(" \"monitor") << idx << string("\"\n");
    }

    os << _end_section("Device");
}

static void _print_screen(ostream& os, const Configuration& config)
{
    int total_width = int(config.seats.size()) * config.seats[0]->get_monitor().get_current_resolution().get_width();

    static const int depth = 24;
    os << _section("Screen")
       << _elem("Identifier")   << string("\"screen0\"")  << endl
       << _elem("Device")       << string("\"card0\"")    << endl
       << _elem("Monitor")      << string("\"monitor0\"") << endl
       << _elem("DefaultDepth") << depth << endl
       << _sub_section("Display")
       << _sub_elem("Depth")    << depth << endl
       << _sub_elem("Virtual")  << total_width << " " << config.seats[0]->get_monitor().get_current_resolution().get_height() << endl
       << _end_sub_section("Display")
       << _end_section("Screen");
}

static void _print_layout(ostream& os, const Configuration& config)
{
    for (uint32_t idx = 0; idx < config.seats.size(); idx++)
    {
        os << _section("ServerLayout")
           << _elem("Identifier") << string("\"seat")       << idx << "\"\n"
           << _elem("Screen")     << string("\"screen0\"\n")
           << _elem("Option")     << string("\"Seat\"\t")   << string("\"seat")
           << idx << "\"\n"
           << _end_section("ServerLayout");
    }
}

void Xorg::configure()
{
    std::stringstream os;
    _print_monitors(os, config);
    _print_device(os,   config);
    _print_screen(os,   config);
    _print_layout(os,   config);
    component_configuration.add(XORG_FILE,
                                "/etc/X11/xorg.conf",
                                Template(QString::fromStdString(os.str())));

    component_configuration.add(XINIT_RC_FILE,
                                "{{home}}/.xinitrc",
                                xorg::prepare_xinitrc_template());
    component_configuration.add(XMST_FILE,
                                "{{home}}/.xmst",
                                xorg::prepare_xmst_template());
}

Template xorg::prepare_xinitrc_template()
{
    return Template_manager::get_instance()->get_template(XINIT_RC_FILE);
}

Template xorg::prepare_xmst_template()
{
    return Template_manager::get_instance()->get_template(XMST_FILE);
}
