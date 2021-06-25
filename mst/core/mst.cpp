#include "mst.h"
#include <QPushButton>
#include <QCoreApplication>
#include <QMainWindow>
#include <QComboBox>
#include <QCheckBox>
#include <memory>
#include <pwd.h> // getpwnam

#include "core/platform.h"
#include "ui/seat_widget/seat_widget.h"
#include "core/types/xrandr_monitor.h"

MST* MST::instance = 0;
Q_LOGGING_CATEGORY(install_controller_category, "mst.core.install_controller")

// constructors ///////////////////////////////////////////////////////////////

MST::MST()
{
    widgets = new vector<QWidget *>;
    list_mice = new QVector<QString>;
    list_keybs = new QVector<QString>;
    current_seat_id = -1;
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
    for (auto xrandr_monitor : availableMonitors) {
        Monitor monitor(xrandr_monitor);
        shared_ptr<Seat> seat = make_shared<Seat>(idx++);
        seat->add_monitor(monitor);
        config->add_seat(seat);
    }
}

void MST::load_seat_configuration_page(QWidget* parent,
                                                     QHBoxLayout* seats_box)
{
    for (auto w : *widgets) {
        delete w;
    }
    widgets->clear();

    list_mice->clear();
    list_keybs->clear();
    Platform::get_input_devices(*list_mice, *list_keybs);
    this->seats_box = seats_box;

    for (auto seat : config->get_seats()) {
        QWidget* widget = new Seat_widget(seat);
        connect(widget, SIGNAL(configure_seat(int)), parent, SLOT(configure_seat(int)));
        widgets->push_back(widget);
        seats_box->addWidget(widget);
    }
}

void MST::prepare_for_device_configuration(int seat_id)
{
    current_seat_id = seat_id;
    config->get_seat(seat_id)->reset_devices();
}

void MST::set_seat_device(QString device, DEVICE_TYPE type)
{
    shared_ptr<Seat> seat = config->get_seat(current_seat_id);
    qInfo(install_controller_category())
            << "Device assigned: " << device << " (" << type << ")";

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

    this->seats_box->update();

    qInfo(install_controller_category()) << seat.get();
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
        try {
            Platform::fs_mkdir(dst.mid(0, dst.lastIndexOf('/')));
        } catch (Platform_exception& e) {
            qWarning(install_controller_category) << e.what();
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
    qInfo(install_controller_category) << "multiseat enabled.";
}

void MST::disable()
{
    component_manager->disable_components();
    qInfo(install_controller_category) << "multiseat disabled.";
}

void MST::create_backup()
{
    try {
        Platform::fs_mkdir(backup_dir);
        component_manager->backup_configurations(backup_dir);
    } catch (Platform_exception& e) {
        qCritical(install_controller_category) << e.what();
        throw MST_exception(e.what());
    }
}

void MST::restore_backup()
{
    component_manager->restore_configurations(backup_dir);
}

void MST::uninstall()
{
    restore_backup();
    disable();
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

QVector<QString> MST::get_mice()
{
    return *list_mice;
}

QVector<QString> MST::get_keyboards()
{
    return *list_keybs;
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
    qInfo(install_controller_category()) << msg;
}
