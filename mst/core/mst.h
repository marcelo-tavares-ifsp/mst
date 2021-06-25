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

/**
 * @brief The MST class
 */
class MST: public QObject
{
    Q_OBJECT

public:
    static MST *get_instance();

    /**
     * @brief set_configuration -- Set MST configuration.
     * @param config
     */
    void set_configuration(Configuration& config);

    /**
     * @brief set_device -- Set device for a seat.
     * @param seat_idx -- Seat index.
     * @param device -- Device name.
     * @param type -- Device type.
     */
    void set_device(int32_t seat_idx, QString device, DEVICE_TYPE type);

    /**
     * @brief load_seats -- Load seats.
     */
    void load_seats();

    /**
     * @brief get_seats -- Get a vector of seats.
     * @return Vector of seats.
     */
    QVector<shared_ptr<Seat>> get_seats() const;

    /**
     * @brief configure -- Configure all the MST components
     *      and store configurations.
     */
    void configure();

    /**
     * @brief stop -- Stop the running MST.
     */
    void stop();

    /**
     * @brief running_p -- Predicate.  Check if MST is running.
     * @return A boolean value.
     */
    bool running_p();

    /**
     * @brief config_is_valid -- Check if the current configuration is valid.
     * @return A boolean value.
     */
    bool config_is_valid();

    /**
     * @brief reset_devices -- Clear all devices from the specified seat.
     * @param seat_id -- Seat to reset.
     */
    void reset_devices(int32_t seat_id);

    /**
     * @brief get_devices -- Get input devices available in the system.
     * @param mice -- An output vector to store mice.
     * @param keyboards -- An output vector to store keyboards.
     */
    void get_devices(QVector<QString>& mice, QVector<QString>& keyboards);

    /**
     * @brief uninstall -- Uninstall MST configuration from the system.
     */
    void uninstall();


    /**
     * @brief install -- Install MST configuration to the system.
     */
    void install();

    /**
     * @brief create_backup -- Create a backup copy of the current configuration
     *      of the system components that will be overwritten by MST during
     *      installation.
     */
    void create_backup();

    /**
     * @brief restore_backup -- Restore the system configuration from a backup
     *      copy.
     */
    void restore_backup();

    /**
     * @brief enable -- Enable MST.
     */
    void enable();

    /**
     * @brief disable -- Disable MST.
     */
    void disable();

    QVector<QString> get_mice();
    QVector<QString> get_keyboards();

private:
    MST();

    void print_config();
    static MST *instance;

    /**
     * @brief config -- Current MST configuration.
     */
    std::shared_ptr<Configuration> config;

    /**
     * @brief component_manager
     */
    Component_manager* component_manager;

    QString backup_dir;
};

#endif // INSTALLCONTROLLER_H
