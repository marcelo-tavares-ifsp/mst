#ifndef DISPLAY_MANAGER_H
#define DISPLAY_MANAGER_H

#include <string>
#include <stdlib.h>
#include <QLoggingCategory>

#include "../component.h"
#include "../configuration.h"
#include "../types/template.h"

Q_DECLARE_LOGGING_CATEGORY(display_manager_category)

namespace display_manager {

//// Constants.

const QString LIGHTDM_FILE = "lightdm-mst.conf";

class Display_manager : public Component
{
public:
    Display_manager(Configuration& config);

    void configure() override;
    void enable() override;
    void disable() override {
        /* Do nothing. */
    }
};


//// Helper procedures.
Template prepare_lightdm_template();

}

#endif // DISPLAY_MANAGER_H
