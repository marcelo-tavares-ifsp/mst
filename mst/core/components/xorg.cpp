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

static QString _make_monitors_section(const Configuration& config)
{
    Template_manager* template_manager = Template_manager::get_instance();
    Template monitor_tpl = template_manager->get_template("xorg/Monitor");
    Template option_tpl  = template_manager->get_template("xorg/Option");
    QString result = "";
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
        result += monitor_tpl.substitute();
    }
    return result;
}

static QString _make_device_section(Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/Device");
    QString result = "";
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++)
    {
        QString interface = config.get_seat(idx)->get_monitor().get_interface();
        result += tpl.set("interface", interface)
                .set("index", QString::number(idx))
                .substitute();
    }
    return result;
}

static QString _make_screen_section(Configuration& config)
{
    int total_width = int(config.get_seat_count()) * config.get_seat(0)->get_monitor().get_current_resolution().get_width();
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/Screen");
    int height = config.get_seat(0)->get_monitor().get_current_resolution()
            .get_height();

    tpl.set("depth", "24")
            .set("width",  QString::number(total_width))
            .set("height", QString::number(height));

    return tpl.substitute();
}

static QString _make_layout_section(Configuration& config)
{
    Template tpl = Template_manager::get_instance()
            ->get_template("xorg/ServerLayout");
    QString result = "";
    for (int32_t idx = 0; idx < config.get_seat_count(); idx++) {
        result += tpl.set("seat_index", QString::number(idx)).substitute();
    }
    return result;
}

void Xorg::configure()
{
    QString xorg_conf = _make_monitors_section(config)
            + _make_device_section(config)
            + _make_screen_section(config)
            + _make_layout_section(config);

    component_configuration.add(XORG_FILE,
                                "/etc/X11/xorg.conf",
                                Template(xorg_conf));

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
