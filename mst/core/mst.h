#ifndef INSTALLCONTROLLER_H
#define INSTALLCONTROLLER_H

#include <QObject>

#include "core/configuration.h"
#include "core/utilites/utilites.h"
#include "core/device/device_listener.h"
#include "core/component_manager.h"
#include "core/components/vgl.h"
#include "core/types/resolution.h"

Q_DECLARE_LOGGING_CATEGORY(install_controller_category)

/**
 * @brief The MST_exception class This class describes an
 *     exception that can be thrown by MST methods.
 */
class MST_exception: public std::runtime_error {
public:
    MST_exception(std::string what)
        : std::runtime_error(what) {
        // Do nothing.
    }
};

class MST: public QObject
{
    Q_OBJECT

public:
    static MST *get_instance();
    void set_configuration(Configuration& config);
    void set_device(int32_t seat_idx, QString device, DEVICE_TYPE type);
    void load_seats();
    QVector<shared_ptr<Seat>> get_seats() const;
    void configure();
    void stop();
    bool running_p();
    bool config_is_valid();
    void reset_devices(int32_t seat_id);
    void get_devices(QVector<QString>& mice, QVector<QString>& keyboards);
    void uninstall();
    void install();
    void create_backup();
    void restore_backup();
    void enable();
    void disable();

    QVector<QString> get_mice();
    QVector<QString> get_keyboards();

private:
    MST();

    void print_config();
    static MST *instance;
    std::shared_ptr<Configuration> config;
    Component_manager* component_manager;

    QString backup_dir;
};

#endif // INSTALLCONTROLLER_H
