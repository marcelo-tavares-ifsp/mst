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
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++)
    {
        os << _section("Monitor")
           << _elem("Identifier") << string("\"") << "monitor" << idx << string("\"\n");

        if (idx < (config.get_seat_count() - 1))
        {
            os << _elem("Option") << "\"LeftOf\"\t" << "\"monitor" << idx + 1 << "\"\n";
        }

        os << _end_section("Monitor");
    }
}

static void _print_device(ostream& os, Configuration& config)
{
    os << _section("Device")
       << _elem("Identifier") << string("\"card0\"\n");

    for (int32_t idx = 0; idx < config.get_seat_count(); idx++)
    {
        os << _elem("Option")
           << string("\"Monitor-") << config.get_seat(idx)->get_monitor().get_interface().toStdString()
           << string("\"")
           << string(" \"monitor") << idx << string("\"\n");
    }

    os << _end_section("Device");
}

static void _print_screen(ostream& os, Configuration& config)
{
    int total_width = int(config.get_seat_count()) * config.get_seat(0)->get_monitor().get_current_resolution().get_width();
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/screen");
    int height = config.get_seat(0)->get_monitor().get_current_resolution()
            .get_height();

    tpl.set("depth", "24")
            .set("width",  QString::number(total_width))
            .set("height", QString::number(height));

    os << tpl.substitute().toStdString();
}

static void _print_layout(ostream& os, Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/ServerLayout");
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++) {
        os << tpl.set("seat_index", QString::number(idx))
              .substitute().toStdString();
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
