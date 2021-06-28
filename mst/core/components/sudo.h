#ifndef SUDO_H
#define SUDO_H

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

namespace sudo {

static const QString SUDOERS_FILE = "sudoers";

class Sudo : public Component
{
public:
    Sudo(Configuration& config);
    void configure() override;
    void disable() override;

    Template prepare_sudoers_template();
};

}

#endif // SUDO_H
