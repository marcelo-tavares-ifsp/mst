#include "installcontroller.h"
#include <QPushButton>
#include <QCoreApplication>
#include <QMainWindow>
#include <QComboBox>
#include <QCheckBox>
#include <memory>

#include "core/platform.h"
#include "ui/seat_widget/seat_widget.h"
#include "core/types/xrandr_monitor.h"

InstallController* InstallController::instance = 0;
Q_LOGGING_CATEGORY(install_controller_category, "mst.install_controller")

// static methods ///////////////////////////////////////////////////////////////

static void add_resolutions_to_cb(vector<string> resolutions, QComboBox *cb)
{
    for(string s : resolutions)
    {
        if (! s.empty())
            cb->addItem(QString::fromStdString(s));
    }
}

/**
 * @brief fill_resolutions_and_interfaces
 * @param cb
 * @param lw
 * @throws InstallController_exception
 */
static void fill_resolutions_and_interfaces(QComboBox *cb, QListWidget *lw)
{
    auto split1 = [] (const string& input, char separator) -> string {
      return split(input, separator)[0];
    };

    auto rcomp = [split1] (const string& left, const string& right) -> int {
            return stoi(split1(left, 'x')) > stoi(split1(right, 'x'));
    };

    cb->clear();
    lw->clear();

    vector<XRandr_monitor> availableMonitors = Platform::xrandr_get_monitors();

    uint32_t am_size = uint32_t(availableMonitors.size());

    if (am_size == 0)
    {
        throw InstallController_exception("Could not get Xrandr output.");
    }

    vector<string> result = availableMonitors[0].resolutions;
    lw->addItem(QString::fromStdString(availableMonitors[0].interface));

    if (am_size > 1)
    {
        for (uint32_t idx = 1; idx < am_size; idx++)
        {
            _set_intersection_x(result, availableMonitors[idx].resolutions, result, rcomp);
            lw->addItem(QString::fromStdString(availableMonitors[idx].interface));
        }
    }

    // TODO: Fix set intersection algorithm to exclude empty strings
    result.erase(remove_if(result.begin(), result.end(), [](const string& elem) {
        return elem.empty();
    }), result.end());

    sort(result.begin(), result.end(), rcomp);
    add_resolutions_to_cb(result, cb);
}

static void clear_layout(QVBoxLayout* vbl, vector<QWidget*> widgets)
{
    for (auto widget : widgets)
    {
        vbl->removeWidget(widget);
        delete widget;
    }
    qInfo(install_controller_category()) << "layout was cleared";
}

static void clear_interface(shared_ptr<Seat> seat)
{
    seat->set_keyboard("");
    seat->set_mouse("");
    seat->set_usb("");
    qInfo(install_controller_category())
            << "interface "
            << seat->get_monitor().get_interface() << " was cleared";
}

// constructors ///////////////////////////////////////////////////////////////

InstallController::InstallController()
{
    config = new Configuration();
    widgets = new vector<QWidget *>;
    list_mice = new vector<string>;
    list_keybs = new vector<string>;
    current_seat_id = -1;
}

// public methods ///////////////////////////////////////////////////////////////

/**
 * @brief InstallController::get_instance -- Get the instance of
 *     InstallController.
 * @return The singleton instance.
 */
InstallController *InstallController::get_instance(){
    if (! instance)
    {
        instance = new InstallController();
    }

    return instance;
}

void InstallController::load_seat_configuration_page(QWidget* parent,
                                                     QHBoxLayout* seats_box)
{
    for (auto w : *widgets) {
        delete w;
    }
    widgets->clear();

    list_mice->clear();
    list_keybs->clear();
    Platform::get_input_devices(*list_mice, *list_keybs);

    vector<XRandr_monitor> availableMonitors = Platform::xrandr_get_monitors();
    int idx = 0;
    for (auto xrandr_monitor : availableMonitors) {
        Monitor monitor(xrandr_monitor);
        shared_ptr<Seat> seat = make_shared<Seat>(idx++);
        seat->add_monitor(monitor);
        config->seats.push_back(seat);
        QWidget* widget = new Seat_widget(seat);
        connect(widget, SIGNAL(configure_seat(int)), parent, SLOT(configure_seat(int)));
        widgets->push_back(widget);
        seats_box->addWidget(widget);
    }
}

void InstallController::prepare_for_device_configuration(int seat_id)
{
    current_seat_id = seat_id;
    clear_interface(config->seats[seat_id]);
}

void InstallController::set_seat_device(QString device, DEVICE_TYPE type)
{
    shared_ptr<Seat> seat = config->seats[current_seat_id];
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

    qInfo(install_controller_category()) << seat.get();
}

QString InstallController::get_instruction(InputDeviceListener * device_listener)
{
    QString instruction = "";

    switch (device_listener->type) {
    case DEVICE_TYPE::KEYBOARD:
        instruction = "Пожалуйста нажимайте на кнопки той клавиатуры, которую хотите использователь для выбранного рабочего места.";
        break;
    case DEVICE_TYPE::MOUSE:
        instruction = "Пожалуйста нажимайте на кнопки той мыши, которую хотите использователь для выбранного рабочего места.";
        break;
    case DEVICE_TYPE::USB:
        instruction = "Пожалуйста вставьте usb-устройство в тот usb разъём, который хотите использователь для выбранного рабочего места.";
        break;
    }

    return instruction;
}

/**
 * @brief InstallController::begin_install -- Configure all the components.
 */
void InstallController::begin_install()
{
    QString out_dir = QString::fromStdString(
                Path_manager::get_instance()->get_output_dir());
    Platform::fs_mkdir(out_dir);
    component_manager = new Component_manager(*config);
    component_manager->configure_components();
    component_manager->store_configurations(out_dir);
}

/**
 * @brief InstallController::install_files -- Install al configuration files.
 */
void InstallController::install_files()
{
    const QString output_dir
            = QString::fromStdString(
                Path_manager::get_instance()->get_output_dir());
    const QString mst_user = Path_manager::get_instance()->get_mst_user();
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
}

void InstallController::enable_mst()
{
    vgl::VGL vgl(*config);
    vgl.enable();
    Platform::system_set_default_runlevel("multi-user");
    qInfo(install_controller_category) << "multiseat enabled.";
}

void InstallController::disable_mst()
{
    vgl::VGL vgl(*config);
    vgl.disable();
    Platform::system_set_default_runlevel("graphical");
    Platform::fs_rm(
                QString::fromStdString(
                    Path_manager::get_instance()->get_sudoers_config()));
    Platform::fs_rm("/etc/bashrc.d/vgl.sh");
}

void InstallController::create_backup()
{
    const QString user = Path_manager::get_instance()->get_mst_user();
    const string usr_dir = Path_manager::get_instance()->get_usr_share_dir();
    const QString cmd =  QString::fromLocal8Bit(INSTALLATION_PREFIX)
            + "/bin/mk_backup.sh " + user;
    if (Platform::exec(cmd) != 0) {
        throw InstallController_exception("Could not create a backup");
    }
}

void InstallController::restore_backup()
{
    const QString user = Path_manager::get_instance()->get_mst_user();
    const QString cmd = QString::fromLocal8Bit(INSTALLATION_PREFIX)
            + "/bin/apl_backup.sh " + user;

    if (system(cmd.toStdString().c_str()))
    {
        qCritical(install_controller_category) << "Could not restore a backup copy.";
        throw InstallController_exception("Could not restore a backup copy.");
    }
}

void InstallController::begin_uninstall()
{
    restore_backup();
    disable_mst();
}

void InstallController::begin_stop()
{
    if (Platform::process_kill("Xephyr"))
    {
        const char* msg = "Could not stop MST ('pkill Xephyr' failed.)";
        qCritical(install_controller_category) << msg;
        throw InstallController_exception(msg);
    }
}

bool InstallController::is_mst_running()
{
    return Platform::process_is_running("Xephyr");
}

void InstallController::set_debug_allow_device_collisions(bool value)
{
    this->debug_allow_device_collisions = value;
}

void InstallController::set_debug_allow_empty_devices(bool value)
{
    this->debug_allow_empty_devices = value;
}

bool InstallController::config_is_valid()
{
    print_config();

    uint32_t count_seats = config->seats.size();
    if (count_seats > 1)
    {
        for (uint32_t i = 0; i < count_seats; i++)
        {
            if (! debug_allow_device_collisions) {
                for (uint32_t j = 1; j < count_seats; j++)
                {
                    if (i == j)
                        continue;
                    if (is_equal(i, j))
                    {
                        qWarning(install_controller_category())
                                << "COLLISION is found";
                        return false;
                    }
                }
            }

            if ((! debug_allow_empty_devices) && is_empty(i))
            {
                qWarning(install_controller_category()) << "EMPTY is found";
                return false;
            }
        }
    }

    return true;
}

bool InstallController::is_equal(int i, int j) {
    QString msg = "Comparison " + QString::number(i) + " seat, "
                      + config->seats[i]->get_keyboard() + " keyboard, "
                      + config->seats[i]->get_mouse() + " mouse, "
                      + config->seats[i]->get_usb() + " usb AND\n"
                      + "\t\t\t\t\t\t\t\t" + QString::number(j) + " seat, "
                      + config->seats[j]->get_keyboard() + " keyboard, "
                      + config->seats[j]->get_mouse() + " mouse, "
                      + config->seats[j]->get_usb() + " usb";
    qInfo(install_controller_category()) << msg;

    return config->seats[i]->intersects(config->seats[j]);
}

bool InstallController::is_empty(int i) {
    QString msg = "";
    bool result = false;

    if (config->seats[i]->get_keyboard() == "")
    {
        msg += "KEYBOARD, ";
        result = true;
    }
    if (config->seats[i]->get_mouse() == "")
    {
        msg += "MOUSE, ";
        result = true;
    }
    if (config->seats[i]->get_usb() == "")
    {
        msg += "USB, ";
        result = true;
    }

    qInfo(install_controller_category()) << msg;

    return result;
}

vector<string> InstallController::get_list_of_mice()
{
    return *list_mice;
}

vector<string> InstallController::get_list_of_keybs()
{
    return *list_keybs;
}

void InstallController::print_config() {
    QString msg = "\n-----START current configuration:\n";

    for (auto seat : config->seats) {
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
