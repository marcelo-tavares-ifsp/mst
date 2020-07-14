#include "configuration.h"

Q_LOGGING_CATEGORY(configuration_category, "mst.core.configuration")

Configuration::Configuration()
{

}

void Configuration::set_debug_allow_device_collisions(bool value)
{
    debug_allow_device_collisions = value;
}

void Configuration::set_debug_allow_empty_devices(bool value)
{
    debug_allow_empty_devices = value;
}

bool Configuration::is_valid()
{
    size_t count_seats = seats.size();
    if (count_seats > 1)
    {
        for (size_t i = 0; i < count_seats; i++)
        {
            if (! debug_allow_device_collisions) {
                for (uint32_t j = 1; j < count_seats; j++)
                {
                    if (i == j)
                        continue;
                    if (seats[i]->intersects(seats[j]))
                    {
                        qWarning(configuration_category())
                                << "COLLISION is found";
                        return false;
                    }
                }
            }

            if ((! debug_allow_empty_devices) && (! seats[i]->is_configured()))
            {
                qWarning(configuration_category()) << "EMPTY is found";
                return false;
            }
        }
    }

    return true;
}
