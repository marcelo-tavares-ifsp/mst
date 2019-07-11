#ifndef INSTALLCONTROLLER_H
#define INSTALLCONTROLLER_H

#include <QObject>

//#include "install_window/installwindow.h"
#include "ui_installwindow.h"
#include "configuration/configuration.h"
#include "command_manager/commandmanager.h"
#include "common/utilites/utilites.h"
#include "input_divece_listener/inputdivecelistener.h"
#include "config_manager/configmanager.h"
#include "config_manager/vgl.h"

Q_DECLARE_LOGGING_CATEGORY(install_controller_category)

class InstallController: public QObject
{
    Q_OBJECT

public:
    static InstallController *get_instance();
    QString get_instruction(InputDeviceListener * device_listener);
    void load_interface_page(QComboBox* cb, QListWidget* lw);
    void save_interfaces(QComboBox* cbResolution, QListWidget* lwMonitors);
    vector<QWidget *> load_device_page(QVBoxLayout* vbl);
    void prepare_for_connect_interface(string name_interface);
    void begin_install();
    void begin_stop();
    void begin_uninstall();
    bool is_mst_running();
    bool config_is_valid();
    void install_files();
    void create_backup();
    void restore_backup();
    void enable_mst();

    vector<string> get_list_of_mice();
    vector<string> get_list_of_keybs();

public slots:
    void set_seat_device(QString, DEVICE_TYPE);

private:
    InstallController();
    bool is_equal(int i, int j);
    bool is_empty(int i);
    void print_config();
    void disable_mst();
    static InstallController *instance;
    Configuration *config;
    vector<QWidget *> *widgets;
    vector<string> *list_mice;
    vector<string> *list_keybs;
    string current_interface_name;
};

#endif // INSTALLCONTROLLER_H