#ifndef DISPLAY_MANAGER_H
#define DISPLAY_MANAGER_H

#include <string>
#include <stdlib.h>
#include <QLoggingCategory>

#include "component.h"
#include "../core/configuration.h"
#include "../core/types/template.h"

Q_DECLARE_LOGGING_CATEGORY(display_manager_category)

namespace display_manager {

//// Constants.

const QString LIGHTDM_FILE = "lightdm-mst.conf";

class Display_manager : public Component
{
public:
    Display_manager(Configuration& config);

    void configure();
    QString get_version() {
        // TODO:
        throw Component_error("Unimplemented");
    }
    void enable();
    void disable() {
        /* Do nothing. */
    }

    void add_seat(int seat_number);
    void add_seats(int count);
};


//// Helper procedures.
Template prepare_lightdm_template();

}

#endif // DISPLAY_MANAGER_H
