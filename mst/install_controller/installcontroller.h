#ifndef INSTALLCONTROLLER_H
#define INSTALLCONTROLLER_H

#include <QObject>
#include <QComboBox>
#include <QListWidget>

#include "ui_installwindow.h"
#include "configuration/configuration.h"
#include "common/utilites/utilites.h"
#include "input_device_listener/inputdevicelistener.h"
#include "components/component_manager.h"
#include "components/vgl.h"

Q_DECLARE_LOGGING_CATEGORY(install_controller_category)

/**
 * @brief The InstallController_exception class This class describes an
 *     exception that can be thrown by InstallController methods.
 */
class InstallController_exception: public runtime_error {
public:
    InstallController_exception(string what)
        : runtime_error(what) {
        // Do nothing.
    }
};

class InstallController: public QObject
{
    Q_OBJECT

public:
    static InstallController *get_instance();
    QString get_instruction(InputDeviceListener * device_listener);
    void load_interface_page(QHBoxLayout* seats_box);
    void save_interfaces(QComboBox* cbResolution, QListWidget* lwMonitors);
    vector<QWidget *> load_device_page(QVBoxLayout* vbl);
    void prepare_for_connect_interface(QString& name_interface);
    void begin_install();
    void begin_stop();
    void begin_uninstall();
    bool is_mst_running();
    bool config_is_valid();
    void install_files();
    void create_backup();
    void restore_backup();
    void enable_mst();
    void disable_mst();

    vector<string> get_list_of_mice();
    vector<string> get_list_of_keybs();

public slots:
    void set_seat_device(QString, DEVICE_TYPE);

private:
    InstallController();
    bool is_equal(int i, int j);
    bool is_empty(int i);
    void print_config();
    static InstallController *instance;
    Configuration *config;
    vector<QWidget *> *widgets;
    vector<string> *list_mice;
    vector<string> *list_keybs;
    QString current_interface_name;
    Component_manager* component_manager;
};

#endif // INSTALLCONTROLLER_H
