#include "configuration.h"

#include <QFile>

using namespace std;

Q_LOGGING_CATEGORY(configuration_category, "mst.core.configuration")

Configuration::Configuration()
{

}

static void create_config_file(QFile& file) {
    if (! file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;
    QTextStream out(&file);
    out << "user:multiseat";
    file.close();
}

/**
 * TODO: Load current seat configuration from a file.
 */
void Configuration::load(QString system_config_file)
{
    QFile file(system_config_file);
    if(! file.exists()) {
        create_config_file(file);
    }
    this->system_config = shared_ptr<DSV>(new DSV(system_config_file.toStdString()));
}

QString Configuration::get_system_mst_user() const
{
    return QString::fromStdString(system_config->get("user"));
}

QString Configuration::get_output_directory() const
{
    QString mst_user = QString::fromStdString(system_config->get("user"));
    return "/home/" + mst_user + "/.local/share/mst/output";
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
