#ifndef SYSTEM_H
#define SYSTEM_H

#include "component.h"
#include "../configuration/configuration.h"
#include "../template_manager/template.h"

namespace sys {

//// Constants.
const QString BASHRC_FILE = "bashrc";
const QString GETTY_FILE  = "getty@.service";

class System : public Component
{
public:
    System(Configuration& config);
    void configure() override;
    QString get_version() override {
        throw Component_error("Unimplemented");
    }
    void enable() {
        /* Do nothing */
    }
    void disable() {
        /* Do nothing. */
    }
};

Template prepare_bashrc_template();
Template prepare_getty_template();

}

#endif // SYSTEM_H