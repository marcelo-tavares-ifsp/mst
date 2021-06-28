#ifndef UDEV_H
#define UDEV_H

#include <QString>

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

namespace udev {

//// Constants.
const QString SYSTEMD_SERVICE_FILE = "systemd-udevd.service";
const QString RULES_FILE           = "99-mst.rules";

//// The main class.

class Udev : public Component
{
public:
    Udev(Configuration& config);
    void configure() override;
};


//// Helper procedures.
Template prepare_systemd_service_template();
QString prepare_udev_rules(Configuration& config);

}

#endif // UDEV_H
