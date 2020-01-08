#include <fstream>

#include "xorg.h"
#include "../configuration/configuration.h"
#include "../template_manager/template.h"
#include "../template_manager/template_manager.h"

using namespace xorg;

Xorg::Xorg(Configuration& config) : Component(config)
{
    config_files[XORG_FILE] = "/etc/X11/xorg.conf";
    config_files[XINIT_RC_FILE] = "{{home}}/.xinitrc";
    config_files[XMST_FILE]     = "{{home}}/.xmst";
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
           << string("\"Monitor-") << config.seats[idx].interface << string("\"")
           << string(" \"monitor") << idx << string("\"\n");
    }

    os << _end_section("Device");
}

static void _print_screen(ostream& os, const Configuration& config)
{
    int total_width = int(config.seats.size()) * config.seats[0].resolution.width;

    static const int depth = 24;
    os << _section("Screen")
       << _elem("Identifier")   << string("\"screen0\"")  << endl
       << _elem("Device")       << string("\"card0\"")    << endl
       << _elem("Monitor")      << string("\"monitor0\"") << endl
       << _elem("DefaultDepth") << depth << endl
       << _sub_section("Display")
       << _sub_elem("Depth")    << depth << endl
       << _sub_elem("Virtual")  << total_width << " " << config.seats[0].resolution.height << endl
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

void Xorg::configure(const QString &output_dir)
{
    QString output_file = output_dir + "/" + XORG_FILE;
    fstream os;
    os.open(output_file.toStdString(), ios::out);
    _print_monitors(os, config);
    _print_device(os,   config);
    _print_screen(os,   config);
    _print_layout(os,   config);

    Template tpl = xorg::prepare_xinitrc_template();
    output_file = output_dir + "/" + XINIT_RC_FILE;
    tpl.substitute(output_file.toStdString());

    tpl = xorg::prepare_xmst_template();
    output_file = output_dir + "/" + XMST_FILE;
    tpl.substitute(output_file.toStdString());
}

Template xorg::prepare_xinitrc_template()
{
    return Template_manager::get_instance()->get_template(
                XINIT_RC_FILE.toStdString());
}

Template xorg::prepare_xmst_template()
{
    return Template_manager::get_instance()->get_template(
                XMST_FILE.toStdString());

}
