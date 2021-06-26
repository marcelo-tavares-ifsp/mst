#include "mst.h"
#include <QCollator>
#include <QDateTime>
#include <QDir>
#include <memory>
#include <pwd.h> // getpwnam

#include "core/platform.h"
#include "core/types/xrandr_monitor.h"

MST* MST::instance = 0;
Q_LOGGING_CATEGORY(mst_category, "mst.core.mst")

// constructors ///////////////////////////////////////////////////////////////

MST::MST()
{

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
    int idx = 0;
    qInfo(mst_category()) << "Adding" << availableMonitors.size() << "seats ...";
    for (auto xrandr_monitor : availableMonitors) {
        qInfo(mst_category())
                << "XRandr monitor:" << QString::fromStdString(xrandr_monitor.interface);
       // qInfo(mst_category()) << config->get_seat(idx);
        shared_ptr<Seat> s = config->get_seat(idx);
        if (s != nullptr) {
            Monitor& monitor = s->get_monitor();
            QString interface = QString::fromStdString(xrandr_monitor.interface);
            if (monitor.get_interface() == interface) {
                qInfo(mst_category())
                        << "  Updating seat" << interface << "...";
                const Resolution& current_resolution
                        = monitor.get_current_resolution();
                monitor.add_resolutions(xrandr_monitor.resolutions);
                int res_idx = 0;
                for (auto res : monitor.get_available_resolutions()) {
                    if (res == current_resolution) {
                        monitor.set_resolution(res_idx);
                        break;
                    }
                    res_idx++;
                }
                qInfo(mst_category())
                        << "  Updating seat" << interface << "... done";
                continue;
            }
        }
        qInfo(mst_category())
                << "  Adding seat"
                << QString::fromStdString(xrandr_monitor.interface)
                << "...";
        Monitor monitor(xrandr_monitor);
        shared_ptr<Seat> seat = make_shared<Seat>(idx++);
        seat->add_monitor(monitor);
        qInfo(mst_category())
                << "  Adding seat"
                << QString::fromStdString(xrandr_monitor.interface)
                << "... done";
        config->add_seat(seat);
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

/**
 * @brief MST::install_files -- Install al configuration files.
 */
void MST::install()
{
    const QString output_dir = config->get_output_directory();
    const QString mst_user = config->get_system_mst_user();
    const QString mst_user_home = "/home/" + mst_user + "/";

    auto install
            = [output_dir](const QString& src, const QString& dst) -> void {
        qInfo(mst_category())
                << "Installing '" + src + "' to '" + dst + "' ...";
        try {
            Platform::fs_mkdir(dst.mid(0, dst.lastIndexOf('/')));
            qInfo(mst_category())
                    << "Installing '" + src + "' to '" + dst + "' ... done";
        } catch (Platform_exception& e) {
            qWarning(mst_category) << e.what();
        }
        Platform::fs_cp(output_dir + "/" + src, dst);
    };

    Platform::fs_mkdir(mst_user_home + ".local/share/mst/output/");
    Platform::fs_mkdir(mst_user_home + ".config/awesome/");
    bool is_pam_mkhomedir_used = Platform::pam_is_mkhomedir_used();
    QString skel = "/etc/skel/";
    struct passwd* pwd = Platform::getpwnam(mst_user);

    vector<Component*> components = component_manager->get_components();
    for (auto comp : components) {
        auto install_paths = comp->get_configuration().get_installation_paths();
        foreach (auto src, install_paths.keys()) {
            QString dst = install_paths[src];
            if (dst.contains("{{home}}")) {
                Template tpl(dst);
                tpl.set("home", mst_user_home);
                install(src, tpl.substitute());
                if (is_pam_mkhomedir_used) {
                    tpl.set("home", skel);
                    install(src, tpl.substitute());
                }
            } else {
                install(src, dst);
            }
        }
    }

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
