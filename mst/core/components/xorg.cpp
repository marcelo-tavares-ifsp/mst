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

/* Config printers. */

static void _print_monitors(ostream& os, const Configuration& config)
{
    Template_manager* template_manager = Template_manager::get_instance();
    Template monitor_tpl = template_manager->get_template("xorg/Monitor");
    Template option_tpl  = template_manager->get_template("xorg/Option");
    option_tpl.set("name", "LeftOf");
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++) {
        monitor_tpl.set("index", QString::number(idx));
        if (idx < (config.get_seat_count() - 1)) {
            monitor_tpl.set("options",
                            option_tpl.set("value", QString::number(idx + 1))
                    .substitute());
        } else {
            monitor_tpl.set("options", "");
        }
        os << monitor_tpl.substitute().toStdString();
    }
}

static void _print_device(ostream& os, Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/Device");

    for (int32_t idx = 0; idx < config.get_seat_count(); idx++)
    {
        QString interface = config.get_seat(idx)->get_monitor().get_interface();
        os << tpl.set("interface", interface)
              .set("index", QString::number(idx))
              .substitute().toStdString();
    }
}

static void _print_screen(ostream& os, Configuration& config)
{
    int total_width = int(config.get_seat_count()) * config.get_seat(0)->get_monitor().get_current_resolution().get_width();
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/Screen");
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
