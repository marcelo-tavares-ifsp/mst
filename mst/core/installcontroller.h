#ifndef INSTALLCONTROLLER_H
#define INSTALLCONTROLLER_H

#include <QObject>
#include <QComboBox>
#include <QListWidget>

#include "ui_installwindow.h"
#include "core/configuration.h"
#include "core/utilites/utilites.h"
#include "core/inputdevicelistener.h"
#include "core/component_manager.h"
#include "core/components/vgl.h"
#include "ui/seat_widget/seat_widget.h"
#include "core/types/resolution.h"

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
    void load_seat_configuration_page(QWidget* parent, QHBoxLayout* seats_box);
    void save_interfaces(QComboBox* cbResolution, QListWidget* lwMonitors);
    vector<QWidget *> load_device_page(QVBoxLayout* vbl);
    void prepare_for_device_configuration(int seat_id);
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

    QVector<string> get_list_of_mice();
    QVector<string> get_list_of_keybs();

    void set_debug_allow_device_collisions(bool value);
    void set_debug_allow_empty_devices(bool value);

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
    QVector<string> *list_mice;
    QVector<string> *list_keybs;
    int current_seat_id;
    Component_manager* component_manager;

    /**
     * @brief debug_allow_device_collisions -- if this option is set, then
     * the controller allows device collisions.
     */
    bool debug_allow_device_collisions = false;

    /**
     * @brief debug_allow_empty_devices -- if this option is set, then
     * the controller allows empty device configurations.
     */
    bool debug_allow_empty_devices = false;
};

#endif // INSTALLCONTROLLER_H