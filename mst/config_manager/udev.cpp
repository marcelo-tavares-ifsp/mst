#include <fstream>

#include "udev.h"

#include "../configuration/configuration.h"
#include "../template_manager/template_manager.h"
#include "../template_manager/template.h"

using namespace udev;

Udev::Udev(Configuration& config) : Component(config)
{
    config_files[SYSTEMD_SERVICE_FILE] = "/etc/systemd/system/"
            + SYSTEMD_SERVICE_FILE;
    config_files[RULES_FILE] = "/etc/udev/rules.d/"
            + RULES_FILE;
}

void Udev::configure(const QString &output_dir)
{
    const QString out_file_name
            = output_dir + "/" + SYSTEMD_SERVICE_FILE;
    Template service_tpl = prepare_systemd_service_template();
    QString rules = prepare_udev_rules(config);
    service_tpl.substitute(out_file_name.toStdString());
    ofstream out(out_file_name.toStdString());
    out << rules.toStdString();
    out.close();
}

Template udev::prepare_systemd_service_template()
{
    Template tpl = Template_manager::get_instance()->get_template(
                SYSTEMD_SERVICE_FILE.toStdString());
    return tpl;
}

QString udev::prepare_udev_rules(Configuration& config)
{
    Template tpl = Template_manager::get_instance()->get_template(
                RULES_FILE.toStdString());
    string result = "";

    for (uint32_t idx = 0; idx < config.seats.size(); ++idx)
    {
        result += tpl.set("usb_device", config.seats[idx].usb)
                   .set("seat_idx", to_string(idx + 1 ))
                   .substitute();
    }

    return QString::fromStdString(result);
}
