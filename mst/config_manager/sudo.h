#ifndef SUDO_H
#define SUDO_H

#include "component.h"
#include "../configuration/configuration.h"
#include "../template_manager/template.h"

namespace sudo {

static const QString SUDOERS_FILE = "sudoers";

class Sudo : public Component
{
public:
    Sudo(Configuration& config);
    void configure(const QString& output_dir);
    QString get_version() {
        // TODO:
        throw Component_error("Unimplemented");
    }
    void enable() {
        /* Do nothing. */
    }
    void disable() {
        /* Do nothing. */
    }
};

//// Helper procedures.

Template prepare_sudoers_template();

}

#endif // SUDO_H
