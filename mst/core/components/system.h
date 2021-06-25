#ifndef SYSTEM_H
#define SYSTEM_H

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

namespace sys {

//// Constants.
const QString BASHRC_FILE = "bashrc";
const QString GETTY_FILE  = "getty@.service";
const QString SEATS_CONFIG = "mst-seats";

class System : public Component
{
public:
    System(Configuration& config);
    void configure() override;
    QString get_version() override {
        throw Component_error("Unimplemented");
    }
    void enable() override;
    void disable() override;
    void stop();

    /**
     * @brief prepare_seat_configuration_template -- Prepare seats configuration
     *     file template.
     * @return A new seats template.
     */
    Template prepare_seat_configuration_template();

    Template prepare_getty_template();
};

Template prepare_bashrc_template();

}

#endif // SYSTEM_H
