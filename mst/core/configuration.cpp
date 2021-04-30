#include "configuration.h"

using namespace std;

Q_LOGGING_CATEGORY(configuration_category, "mst.core.configuration")

Configuration::Configuration()
{

}

void Configuration::add_seat(shared_ptr<Seat> seat)
{
    seats.push_back(seat);
}

int32_t Configuration::get_seat_count() const
{
    return seats.size();
}

shared_ptr<Seat> Configuration::get_seat(int32_t idx)
{
    return seats[idx];
}

QVector<shared_ptr<Seat>> Configuration::get_seats()
{
    return seats;
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
                                << "Collision is found: "
                                << seats[i].get() << " intersects " << seats[j].get();
                        return false;
                    }
                }
            }

            if ((! debug_allow_empty_devices) && (! seats[i]->is_configured()))
            {
                qWarning(configuration_category()) << "Seat is not properly configured: "
                                                   << seats[i].get();
                return false;
            }
        }
    }

    return true;
}
