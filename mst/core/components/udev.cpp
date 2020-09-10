#include <fstream>

#include "udev.h"

#include "../configuration.h"
#include "core/template_manager.h"
#include "core/types/template.h"

using namespace udev;

Udev::Udev(Configuration& config) : Component(config)
{
    /* Do nothing. */
}

void Udev::configure()
{
    component_configuration.add(SYSTEMD_SERVICE_FILE,
                                "/etc/systemd/system/",
                                prepare_systemd_service_template());
    component_configuration.add(RULES_FILE,
                                "/etc/udev/rules.d/",
                                Template(prepare_udev_rules(config)));
}

Template udev::prepare_systemd_service_template()
{
    Template tpl = Template_manager::get_instance()->get_template(
                SYSTEMD_SERVICE_FILE);
    return tpl;
}

QString udev::prepare_udev_rules(Configuration& config)
{
    Template tpl = Template_manager::get_instance()->get_template(RULES_FILE);
    QString result = "";

    for (int32_t idx = 0; idx < config.get_seat_count(); ++idx)
    {
        result += tpl.set("usb_device", config.get_seat(idx)->get_usb() + "/*")
                .set("prefix", QString::fromLocal8Bit(INSTALLATION_PREFIX))
                .set("seat_idx", QString::number(idx + 1 ))
                .substitute();
    }

    return result;
}
