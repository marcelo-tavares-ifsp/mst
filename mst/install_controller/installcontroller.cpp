#include "installcontroller.h"
#include <QPushButton>
#include <QCoreApplication>
#include <QMainWindow>

#include "../platform/platform.h"

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

    vector<xrandrMonitor> availableMonitors = Platform::xrandr_get_monitors();

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

static void clear_interface(string name_interface, vector<Seat> seats)
{
    for (uint32_t i = 0; i < seats.size(); i++)
    {
        if (seats[i].interface == name_interface)
        {
            seats[i].keyboard = "";
            seats[i].mouse = "";
            seats[i].usb = "";
            qInfo(install_controller_category()) << "interface " << name_interface.c_str() << " was cleared";
            return;
        }
    }
}

// constructors ///////////////////////////////////////////////////////////////

InstallController::InstallController()
{
    config = new Configuration();
    widgets = new vector<QWidget *>;
    list_mice = new vector<string>;
    list_keybs = new vector<string>;
    current_interface_name = "";
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

void InstallController::load_interface_page(QComboBox *cb, QListWidget *lw)
{
    fill_resolutions_and_interfaces(cb, lw);
    cb->setCurrentIndex(cb->count() - 1);
    lw->selectAll();
}

void InstallController::save_interfaces(QComboBox* cbResolution, QListWidget* lwMonitors)
{
    config->seats.clear();

    QList<QListWidgetItem*> items = lwMonitors->selectedItems();
    vector<int> resolution = _parse_resolution(cbResolution->currentText()); // TODO: individual resolution for each monitor

    qInfo(install_controller_category()) << "Settings for monitors:";
    for (int idx = 0; idx < items.count(); idx++)
    {
        Seat seat;
        seat.interface = to_std_string(items[idx]->text());
        seat.resolution.width = resolution[0];
        seat.resolution.height = resolution[1];
        config->seats.push_back(seat);
        qInfo(install_controller_category()) << "Name: " << seat.interface.c_str();
        qInfo(install_controller_category()) << "width: " << seat.resolution.width
                                             << "; height: " << seat.resolution.height;
    }
}

vector<QWidget *> InstallController::load_device_page(QVBoxLayout* vbl)
{
    clear_layout(vbl, *widgets);
    widgets->clear();

    for (auto seat : config->seats)
    {
        QPushButton *btn = new QPushButton(to_qstring(seat.interface));
        qInfo(install_controller_category()) << "Button: " << seat.interface.c_str() << " was added";
        btn->setFocusPolicy(Qt::NoFocus);
        widgets->push_back(btn);
    }

    list_mice->clear();
    list_keybs->clear();
    Platform::get_input_devices(*list_mice, *list_keybs);

    return *widgets;
}

void InstallController::prepare_for_connect_interface(string name_interface)
{
    current_interface_name = name_interface;
    clear_interface(name_interface, config->seats);
}

void InstallController::set_seat_device(QString device, DEVICE_TYPE type)
{
    // TODO: red and after detecring green mouse and keyboard =)

    string d = to_std_string(device);

    qInfo(install_controller_category())
            << "Device assigned: " << d.c_str() << " (" << type << ")";

    for (uint32_t i = 0; i < config->seats.size(); i++)
    {
        if (config->seats[i].interface == current_interface_name)
        {
            switch (type) {
            case DEVICE_TYPE::KEYBOARD:
                config->seats[i].keyboard = d;
                break;
            case DEVICE_TYPE::MOUSE:
                config->seats[i].mouse = d;
                break;
            case DEVICE_TYPE::USB:
                config->seats[i].usb = d;
                break;
            }

            qInfo(install_controller_category())
                    << "Seat interface: '" << config->seats[i].interface.c_str()
                    << "'; keyboard: '" << config->seats[i].keyboard.c_str()
                    << "'; mouse: '" << config->seats[i].mouse.c_str()
                    << "'; usb: " << config->seats[i].usb.c_str() << "'";
        }
    }
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
                PathManager::get_instance()->get_output_dir());
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
                PathManager::get_instance()->get_output_dir());
    const QString mst_user = PathManager::get_instance()->get_mst_user();
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
                    PathManager::get_instance()->get_sudoers_config()));
    Platform::fs_rm("/etc/bashrc.d/vgl.sh");
}

void InstallController::create_backup()
{
    const QString user = PathManager::get_instance()->get_mst_user();
    const string usr_dir = PathManager::get_instance()->get_usr_share_dir();
    const QString cmd =  QString::fromLocal8Bit(INSTALLATION_PREFIX)
            + "/bin/mk_backup.sh " + user;
    if (Platform::exec(cmd) != 0) {
        throw InstallController_exception("Could not create a backup");
    }
}

void InstallController::restore_backup()
{
    const QString user = PathManager::get_instance()->get_mst_user();
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

bool InstallController::config_is_valid()
{
    print_config();

    uint32_t count_seats = config->seats.size();
    if (count_seats > 1)
    {
        for (uint32_t i = 0; i < count_seats; i++)
        {
            for (uint32_t j = 1; j < count_seats; j++)
            {
                if (i == j)
                    continue;

                if (is_equal(i, j))
                {
                    qWarning(install_controller_category()) << "COLLISION is found";
                    return false;
                }
            }

            if (is_empty(i))
            {
                qWarning(install_controller_category()) << "EMPTY is found";
                return false;
            }
        }
    }

    return true;
}

bool InstallController::is_equal(int i, int j) {
    string msg = "Comparison " + std::to_string(i) + string(" seat, ")
                      + config->seats[i].keyboard + " keyboard, "
                      + config->seats[i].mouse + " mouse, "
                      + config->seats[i].usb + " usb AND\n"
                      + string("\t\t\t\t\t\t\t\t") + std::to_string(j) + " seat, "
                      + config->seats[j].keyboard + " keyboard, "
                      + config->seats[j].mouse + " mouse, "
                      + config->seats[j].usb + " usb";
    qInfo(install_controller_category()) << msg.c_str();

    return ((config->seats[i].keyboard == config->seats[j].keyboard)
            || (config->seats[i].mouse == config->seats[j].mouse)
                || (config->seats[i].usb == config->seats[j].usb));
}

bool InstallController::is_empty(int i) {
    string msg = "";
    bool result = false;

    if (config->seats[i].keyboard == "")
    {
        msg += "KEYBOARD, ";
        result = true;
    }
    if (config->seats[i].mouse == "")
    {
        msg += "MOUSE, ";
        result = true;
    }
    if (config->seats[i].usb == "")
    {
        msg += "USB, ";
        result = true;
    }

    qInfo(install_controller_category()) << msg.c_str();

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
    string msg = "\n-----START current configuration:\n";

    for (auto seat : config->seats) {
        msg += "\tseat: ";
        msg += seat.interface + "\n";
        msg += "\tkeyboard: ";
        msg += seat.keyboard + "\n";
        msg += "\tmouse: ";
        msg += seat.mouse + "\n";
        msg += "\tusb: ";
        msg += seat.usb + "\n";
        msg += "\tresolution: ";
        msg += std::to_string(seat.resolution.width);
        msg += "x";
        msg += std::to_string(seat.resolution.height) + "\n";
    }

    msg += "-----END current configuration";
    qInfo(install_controller_category()) << msg.c_str();
}
