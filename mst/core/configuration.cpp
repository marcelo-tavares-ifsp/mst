#include "configuration.h"

#include <QFile>

using namespace std;

Q_LOGGING_CATEGORY(configuration_category, "mst.core.configuration")

Configuration::Configuration()
{

}

static bool create_config_file(QFile& file) {
    if (! file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        return false;
    }
    QTextStream out(&file);
    out << "user:multiseat" << endl;
    file.close();
    return true;
}

/**
 * TODO: Load current seat configuration from a file.
 */
void Configuration::load(QString system_config_file, QString seats_config_file)
{
    QFile file(system_config_file);
    if(! file.exists()) {
        qInfo(configuration_category())
                << "Creating default configuration file"
                << "'" + system_config_file +  "' ...";
        if (create_config_file(file)) {
            qInfo(configuration_category())
                    << "Creating default configuration file"
                    << "'" + system_config_file + "' ... done";
        } else {
            qCritical(configuration_category())
                    << "Could not create configuration file:"
                    << system_config_file;
            qInfo(configuration_category())
                    << "Creating default configuration file"
                    << "'" + system_config_file + "' ... FAIL";
        }
    }
    this->system_config = make_shared<DSV>(system_config_file.toStdString());

    QFile seats_config(seats_config_file);

    if (seats_config.exists() && seats_config.open(QIODevice::ReadOnly)) {
        qInfo(configuration_category())
                << "Loading seats configuration from"
                << "'" + seats_config_file + "' ...";
        QTextStream stream(&seats_config);
        for (QString line = stream.readLine();
             (! line.isNull());
             line = stream.readLine()) {
            qInfo(configuration_category())
                    << "  Parsing line:" << line;
            QStringList params = line.split(" ");
            shared_ptr<Seat> seat = make_shared<Seat>(params[0].toInt() - 1);
            QVector<Resolution> resolutions;
            try {
                resolutions.push_back(params[2]);
                Monitor monitor(params[1], resolutions);
                seat->add_monitor(monitor);
                seat->set_keyboard(params[3]);
                seat->set_mouse(params[4]);
                if (params.size() == 6) {
                    seat->set_usb(params[5]);
                }
                add_seat(seat);
            } catch (Resolution_error& error) {
                QString msg = "Could not parse the line \""
                        + line + "\": Resolution_error: "
                        + error.what();
                qCritical(configuration_category()) << msg;
                throw Configuration_error(msg);
            }
        }
        seats_config.close();
        qInfo(configuration_category())
                << "Loading seats configuration from"
                << "'" + seats_config_file + "' ... done";
    }
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
    int32_t count = seats.size();
    if ((count > 0) && (idx >= 0) && (idx < count))
        return seats[idx];
    else
        return nullptr;
}

QVector<shared_ptr<Seat>> Configuration::get_seats()
{
    return seats;
}

void Configuration::allow_device_collisions(bool value)
{
    device_collisions_allowed_p = value;
}

void Configuration::allow_empty_devices(bool value)
{
    empty_devices_allowed_p = value;
}

bool Configuration::is_valid()
{
    size_t count_seats = seats.size();
    if (count_seats > 1) {
        for (size_t i = 0; i < count_seats; i++) {
            if (! device_collisions_allowed_p) {
                for (uint32_t j = 1; j < count_seats; j++) {
                    if (i == j)
                        continue;

                    if (seats[i]->intersects(seats[j])) {
                        qWarning(configuration_category())
                                << "Collision is found: "
                                << seats[i].get()
                                << " intersects "
                                << seats[j].get();
                        return false;
                    }
                }
            }

            if ((! empty_devices_allowed_p) && (! seats[i]->is_configured())) {
                qWarning(configuration_category())
                        << "Seat is not properly configured: "
                        << seats[i].get();
                return false;
            }
        }
        return true;
    } else if (count_seats == 1) {
        return seats[0]->is_configured();
    } else {
        return false;
    }
}
