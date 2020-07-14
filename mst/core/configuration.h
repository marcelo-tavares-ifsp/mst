#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <memory>
#include <vector>
#include <QLoggingCategory>
#include "core/types/seat.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

class Configuration
{
public:
    Configuration();

    void set_debug_allow_device_collisions(bool value);
    void set_debug_allow_empty_devices(bool value);

    /**
     * @brief is_valid -- Predicate.  Check if the configuration is valid.
     * @return true if it is, false otherwise.
     */
    bool is_valid();

    vector<shared_ptr<Seat>> seats;

private:
    /**
     * @brief debug_allow_device_collisions -- if this option is set, then
     * the controller allows device collisions.
     */
    bool debug_allow_device_collisions = false;

    /**
     * @brief debug_allow_empty_devices -- if this option is set, then
     * the controller allows empty device configurations.
     */
    bool debug_allow_empty_devices = false;
};

#endif // CONFIGURATION_H
