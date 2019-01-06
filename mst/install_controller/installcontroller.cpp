#include "installcontroller.h"
#include <QPushButton>
#include <QCoreApplication>
#include <QMainWindow>

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

static void fill_resolutions_and_interfaces(QComboBox *cb, QListWidget *lw)
{
    cb->clear();
    lw->clear();

    vector<xrandrMonitor> availableMonitors = CommandManager::get_interfaces_from_xrandr();

    uint32_t am_size = uint32_t(availableMonitors.size());
    vector<string> resolutions;
    vector<string>::iterator it;

    if (am_size == 0)
    {
        throw "Could not get Xrandr output.";
    }

    resolutions = availableMonitors[0].resolutions;
    lw->addItem(QString::fromStdString(availableMonitors[0].interface));

    if(am_size > 1)
    {
        for (uint32_t idx = 1; idx < am_size; idx++)
        {
            sort(resolutions.begin(), resolutions.end());
            sort(availableMonitors[idx].resolutions.begin(), availableMonitors[idx].resolutions.end());
            it = _set_intersection(resolutions, availableMonitors[idx].resolutions, resolutions);
            resolutions.resize(it->size());
            lw->addItem(QString::fromStdString(availableMonitors[idx].interface));
        }
    }

    add_resolutions_to_cb(resolutions, cb);
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
    CommandManager::get_devices_from_ls(*list_mice, *list_keybs);

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

void InstallController::begin_install()
{
    ConfigManager::make_rc_lua(*config);
    qInfo(install_controller_category) << "make_rc_lua OK";
    ConfigManager::make_xorg(*config);
    qInfo(install_controller_category) << "make_xorg OK";
    ConfigManager::make_bashrc();
    qInfo(install_controller_category) << "make_bashrc OK";
    ConfigManager::make_xinitrc();
    qInfo(install_controller_category) << "make_xinitrc OK";
    ConfigManager::make_sudoers();
    qInfo(install_controller_category) << "make_sudoers OK";
    ConfigManager::make_lightdm_conf();
    qInfo(install_controller_category) << "make_lightdm_conf OK";
    ConfigManager::make_getty_service();
    qInfo(install_controller_category) << "make_getty_service OK";
    ConfigManager::make_udev_rules(config->seats);
    qInfo(install_controller_category) << "make_udev_rules OK";
    ConfigManager::make_udev_service();
    qInfo(install_controller_category) << "make_udev_service OK";
    ConfigManager::make_vgl();
    qInfo(install_controller_category) << "make_vgl OK";
}

void InstallController::install_files()
{
    const string output_dir = PathManager::get_output_dir();
    const string mst_user   = PathManager::get_mst_user();
    const string mst_user_home = "/home/" + mst_user + "/";

    auto install = [output_dir](const string& src, const string& dst) -> void {
      cp(output_dir + "/" + src, dst);
    };

    string cmd = "mkdir -p " + mst_user_home + ".local/share/mst/output/";
    if (system(cmd.c_str())) {
        qCritical(install_controller_category)
             << "Could not create a directory: " << cmd.c_str();
        throw "Could not create a directory: " + cmd;
    }

    cmd = "mkdir -p " + mst_user_home + ".config/awesome/";
    if (system(cmd.c_str()))
    {
        qCritical(install_controller_category)
             << "Could not create a directory: " << cmd.c_str();
        throw "Could not create a directory: " + cmd;
    }

    install("rc.lua",    mst_user_home + ".config/awesome/");
    install("xorg.conf", "/etc/X11/xorg.conf");
    install(".bashrc",   mst_user_home);
    install(".xinitrc",   mst_user_home);
    install(".xmst",      mst_user_home);
    install("vgl.sh",     "/etc/bashrc.d/");
    install("lightdm-mst.conf", "/etc/lightdm/");
    install("getty@.service",   "/lib/systemd/system/getty@.service");
    if (is_pam_mkhomedir_used())
    {
        string skel = "/etc/skel";
        cmd = "mkdir -p " + skel + "/.config/awesome/";
        if (system(cmd.c_str()))
        {
            qCritical(install_controller_category)
                    << "Could not create a directory: " << cmd.c_str();
            throw "Could not create a directory: " + cmd;
        }
        install("rc.lua",    skel + "/.config/awesome/");
        install(".bashrc",   skel);
        install(".xinitrc",   skel);
        install(".xmst",      skel);
    }
    install("sudoers",   PathManager::get_sudoers_d_config());

    install("99-mst.rules", "/etc/udev/rules.d/99-mst.rules");
    install("systemd-udevd.service", "/etc/systemd/system");
}

void InstallController::enable_mst()
{
    VGL::configure();
    if (system("systemctl set-default multi-user.target"))
        {
            qCritical(install_controller_category) << "Could not enable MST in systemd.";
            throw "Could not enable MST in systemd.";
        }
    qInfo(install_controller_category) << "multiseat enabled.";
}

void InstallController::disable_mst()
{
    VGL::unconfigure();
    if (system("systemctl set-default graphical.target"))
    {
        qCritical(install_controller_category) << "Could not disable MST in systemd.";
        throw "Could not disable MST in systemd.";
    }

    string cmd = "rm '" + PathManager::get_sudoers_config() + "'";

    if (system(cmd.c_str()))
    {
        string message = "Could not delete "
                + PathManager::get_sudoers_config() + ".";
        qCritical(install_controller_category) << message.c_str();
        throw message;
    }

    cmd = "rm '/etc/bashrc.d/vgl.sh'";
    if (system(cmd.c_str()))
    {
        string message = "Could not delete "
                + PathManager::get_vgl_config() + ".";
        qCritical(install_controller_category) << message.c_str();
        throw message;
    }
}

void InstallController::create_backup()
{
    const string user = PathManager::get_mst_user();
    const string usr_dir = PathManager::get_usr_share_dir();
    const string cmd = "/usr/local/bin/mk_backup.sh " + user;
    if (system(cmd.c_str()))
    {
        qCritical(install_controller_category)
                << "Could not create a backup: "
                << cmd.c_str();
        throw "Could not create a backup: " + cmd;
    }
}

void InstallController::restore_backup()
{
    const string user = PathManager::get_mst_user();
    const string cmd = "/usr/local/bin/apl_backup.sh " + user;

    if (system(cmd.c_str()))
    {
        qCritical(install_controller_category) << "Could not restore a backup copy.";
        throw "Could not restore a backup copy.";
    }
}

void InstallController::begin_uninstall()
{
    restore_backup();
    disable_mst();
}

void InstallController::begin_stop()
{
    const char processName[7] = "Xephyr";
    if (CommandManager::kill_process(processName))
    {
        const char* msg = "Could not stop MST ('pkill Xephyr' failed.)";
        qCritical(install_controller_category) << msg;
        throw msg;
    }
}

bool InstallController::is_mst_running()
{
    const char processName[7] = "Xephyr";
    return CommandManager::is_running(processName);
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

    msg += "-----END currecnt cunfiguration";
    qInfo(install_controller_category()) << msg.c_str();
}
