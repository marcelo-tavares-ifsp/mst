/* mst.cpp -- Core MST class implementation.
 *
 * Copyright (C) 2018-2022 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2018-2022 Artyom V. Poptsov <a@gkaz.ru>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "mst.h"
#include <QCollator>
#include <QDateTime>
#include <QDir>
#include <memory>
#include <pwd.h> // getpwnam

#include "config.h"
#include "core/platform.h"
#include "core/types/xrandr_monitor.h"

MST* MST::instance = 0;
Q_LOGGING_CATEGORY(mst_category, "mst.core.mst")

// constructors ///////////////////////////////////////////////////////////////

MST::MST()
{
    qInfo(mst_category())
            << "Running on:" << Platform::system_release() << Qt::endl;
}

// public methods ///////////////////////////////////////////////////////////////

void MST::set_configuration(Configuration &config)
{
    this->config = std::shared_ptr<Configuration>(&config);
    this->backup_dir = "/home/" + config.get_system_mst_user()
            + "/.local/share/mst/backup/";
    component_manager = new Component_manager(config);
}

/**
 * @brief MST::get_instance -- Get the instance of
 *     MST.
 * @return The singleton instance.
 */
MST *MST::get_instance(){
    if (! instance)
    {
        instance = new MST();
    }

    return instance;
}

void MST::load_seats()
{
    vector<XRandr_monitor> availableMonitors = Platform::xrandr_get_monitors();
    int seat_number = 1;
    qInfo(mst_category()) << "Adding" << availableMonitors.size() << "seats ...";
    for (auto xrandr_monitor : availableMonitors) {
        qInfo(mst_category())
                << "XRandr monitor:" << xrandr_monitor.interface;
       // qInfo(mst_category()) << config->get_seat(idx);
        shared_ptr<Seat> s = config->get_seat(seat_number);
        if (s != nullptr) {
            Monitor& monitor = s->get_monitor();
            QString interface = xrandr_monitor.interface;
            qInfo(mst_category())
                    << "  Updating seat" << interface << "...";
            if (monitor.get_interface() == interface) {
                qInfo(mst_category())
                        << "    Updating a monitor "
                        << interface << " ... ";
                const Resolution& current_resolution
                        = monitor.get_current_resolution();
                monitor.add_resolutions(xrandr_monitor.resolutions);
                monitor.set_resolution(current_resolution);
                qInfo(mst_category())
                        << "    Updating a monitor "
                        << interface << " ... done";
            } else {
                qInfo(mst_category())
                        << "    Replacing a monitor "
                        << monitor.get_interface()
                        << " with "
                        << interface << " ... ";
                s->remove_monitor(monitor);
                Monitor new_monitor(xrandr_monitor);
                s->add_monitor(new_monitor);
                qInfo(mst_category())
                        << "    Replacing a monitor "
                        << monitor.get_interface()
                        << " with "
                        << interface << " ... done";
            }
            qInfo(mst_category())
                    << "  Updating seat" << interface << "... done";
        } else {
            qInfo(mst_category())
                    << "  Adding seat"
                    << xrandr_monitor.interface
                    << "...";
            Monitor monitor(xrandr_monitor);
            shared_ptr<Seat> seat = make_shared<Seat>(seat_number);
            seat->add_monitor(monitor);
            qInfo(mst_category())
                    << "  Adding seat"
                    << xrandr_monitor.interface
                    << "... done";
            config->add_seat(seat);
        }
        seat_number++;
    }
    qInfo(mst_category()) << "Adding" << availableMonitors.size() << "seats ... done";
}

QVector<shared_ptr<Seat>> MST::get_seats() const
{
    return config->get_seats();
}

void MST::reset_devices(int32_t seat_id)
{
    config->get_seat(seat_id)->reset_devices();
}

void MST::set_device(int32_t seat_idx, QString device, DEVICE_TYPE type)
{
    shared_ptr<Seat> seat = config->get_seat(seat_idx);

    switch (type) {
    case DEVICE_TYPE::KEYBOARD:
        seat->set_keyboard(device);
        break;
    case DEVICE_TYPE::MOUSE:
        seat->set_mouse(device);
        break;
    case DEVICE_TYPE::USB:
        seat->set_usb(device);
        break;
    }

    qInfo(mst_category())
            << "Device assigned: " << device << " (" << type << ")";
    qInfo(mst_category()) << seat.get();
}


/**
 * @brief MST::configure -- Configure all the components.
 */
void MST::configure()
{
    QString out_dir = config->get_output_directory();
    Platform::fs_mkdir(out_dir);
    component_manager->configure_components();
    component_manager->store_configurations(out_dir);
}

void MST::create_multiseat_user()
{
    const QString mst_user = config->get_system_mst_user();
    struct passwd* pwd = Platform::getpwnam(mst_user);
    if (pwd) {
        QString message = "User '" + mst_user + "' already exists";
        qInfo(mst_category()).noquote() << message;
        return;
    }

    qInfo(mst_category()).noquote()
            << "Creating user '" + mst_user + "' ...";
    int rc = Platform::exec("useradd -m -s /bin/bash '" + mst_user + "'");
    if (rc != 0) {
        QString message = "Could not create a user '" + mst_user + "'";
        qCritical(mst_category()).noquote() << message;
        throw MST_exception(message);
    }
    qInfo(mst_category()).noquote()
            << "Creating user '" + mst_user + "' ... done";

    QVector<QString> groups = {
        "wheel", "docker", "video", "render", "sudo"
    };

    qInfo(mst_category()).noquote()
            << "Adding '" + mst_user + "' to groups ...";

    for (auto group : groups) {
        qInfo(mst_category()).noquote()
                << "  Adding '" + mst_user + "' to '" + group + "'";
        rc = Platform::exec("usermod -a -G " + group + " '" + mst_user + "'");
        if (rc != 0) {
            QString message = "Could add user '" + mst_user + "' to group '" + group + "'";
            qWarning(mst_category()).noquote() << message;
        }
    }

    qInfo(mst_category()).noquote()
            << "Adding '" + mst_user + "' to groups ... done";
}

/**
 * @brief MST::install_files -- Install al configuration files.
 * @throws MST_exception on errors.
 */
void MST::install()
{
    create_multiseat_user();

    const QString mst_user = config->get_system_mst_user();
    struct passwd* pwd = Platform::getpwnam(mst_user);
    if (! pwd) {
        QString message = "User '" + mst_user + "' is not found";
        qCritical(mst_category()).noquote() << message;
        throw MST_exception(message);
    }

    const QString mst_user_home = "/home/" + mst_user + "/";
    Platform::fs_mkdir(mst_user_home + ".local/share/mst/output/");

    component_manager->install_components();

    Platform::chown(mst_user_home, pwd->pw_uid, pwd->pw_gid, true);
}

void MST::enable()
{
    component_manager->enable_components();
    qInfo(mst_category) << "multiseat enabled.";
}

void MST::disable()
{
    component_manager->disable_components();
    qInfo(mst_category) << "multiseat disabled.";
}

void MST::create_backup()
{
    QString timestamp = QDateTime::currentDateTime()
            .toString(Qt::DateFormat::ISODate)
            .replace(":", "-");
    QString current_backup_dir = backup_dir + "/" + timestamp + "/";
    try {
        Platform::fs_mkdir(current_backup_dir);
        component_manager->backup_configurations(current_backup_dir);
    } catch (Platform_exception& e) {
        qCritical(mst_category) << e.what();
        throw MST_exception(e.what());
    }
}

QStringList MST::list_backups() const
{
    QDir dir(backup_dir);
    dir.setFilter(QDir::Dirs | QDir::NoSymLinks | QDir::NoDotAndDotDot);
    dir.setSorting(QDir::NoSort);

    QCollator collator;
    collator.setNumericMode(true);

    QStringList list = dir.entryList();
    sort(list.begin(), list.end(), collator);
    return list;
}

void MST::restore_backup(QString backup_name)
{
    QString backup_path = backup_dir + backup_name + "/";
    qInfo(mst_category())
            << "Restoring backup '" << backup_path << "' ...";
    component_manager->restore_configurations(backup_path);
    qInfo(mst_category())
            << "Restoring backup '" << backup_path << "' ... done";
}

void MST::uninstall()
{
    disable();
    QStringList list =  list_backups();
    restore_backup(list.last());
}

void MST::start()
{
    component_manager->start_components();
}

void MST::stop()
{
    component_manager->stop_components();
}

bool MST::running_p()
{
    return Platform::system_service_active_p("mstd");
}

bool MST::config_is_valid()
{
    print_config();

    return config->is_valid();
}

void MST::get_devices(QVector<QString>& mice, QVector<QString>& keyboards)
{
    Platform::get_input_devices(mice, keyboards);
}

void MST::print_config() {
    QString msg = "\n-----START current configuration:\n";

    for (auto seat : config->get_seats()) {
        msg += "\tseat: ";
        msg += seat->get_monitor().get_interface() + "\n";
        msg += "\tkeyboard: ";
        msg += seat->get_keyboard() + "\n";
        msg += "\tmouse: ";
        msg += seat->get_mouse() + "\n";
        msg += "\tusb: ";
        msg += seat->get_usb() + "\n";
        msg += "\tresolution: ";
        msg += QString::number(seat->get_monitor().get_current_resolution().get_width());
        msg += "x";
        msg += QString::number(seat->get_monitor().get_current_resolution().get_height()) + "\n";
    }

    msg += "-----END current configuration";
    qInfo(mst_category()) << msg;
}

QString MST::get_backup_directory() const
{
    return backup_dir;
}
