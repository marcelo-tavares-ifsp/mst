#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <memory>
#include <vector>
#include <QLoggingCategory>
#include "core/dsv_parser/dsv.h"
#include "core/types/seat.h"

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

/**
 * @brief The Configuration_error class -- describes a Configuration error.
 */
class Configuration_error : public std::runtime_error {
public:
    Configuration_error(QString what)
        : std::runtime_error(what.toStdString()) {
        // Do nothing.
    }
};

/**
 * @brief The Configuration class describes MST configuration.
 */
class Configuration
{
public:
    Configuration();

    /**
     * @brief load -- Load configuration from a file.
     * @param system_config_file -- Name of the file to load.
     * @param seats_config_file -- Name of the seats configuration file to load.
     */
    void load(QString system_config_file, QString seats_config_file);

    /**
     * @brief allow_device_collisions -- Set whether device collisions
     *      should be allowed in the configuration or not.
     *
     * Useful for debugging purposes.
     *
     * @param value -- 'true' or 'false'.
     */
    void allow_device_collisions(bool value);

    /**
     * @brief allow_empty_devices -- Set whether empty (non-configured)
     *      devices should be allowed in the configuration or not.
     *
     * Useful for debugging purposes.
     *
     * @param value -- 'true' or 'false'.
     */
    void allow_empty_devices(bool value);

    /**
     * @brief allow_missing_components -- Set whether missing MST components
     *     (e.g. Awesome TWM) are allowed or not.
     *
     * Useful for debugging purposes.
     *
     * @param value -- 'true' or 'false'.
     */
    void allow_missing_components(bool value);

    /**
     * @brief debug_missing_components_allowed_p -- return 'true' if missing
     *     components are allowed.
     * @return a boolean value;
     */
    bool debug_missing_components_allowed_p() const;

    /**
     * @brief is_valid -- Predicate.  Check if the configuration is valid.
     * @return true if it is, false otherwise.
     */
    bool is_valid();

    /**
     * @brief add_seat -- Add a new seat.
     * @param seat -- A Seat instance.
     */
    void add_seat(std::shared_ptr<Seat> seat);

    /**
     * @brief get_seat_count -- Get number of added seats.
     * @return the number of seats.
     */
    int32_t get_seat_count() const;

    /**
     * @brief get_seat -- Get a seat.
     * @param seat_number -- A seat number.  Seat numbering starts from one.
     * @return A shared pointer to a seat.
     * @pre seat_number is greater than zero.
     */
    std::shared_ptr<Seat> get_seat(int32_t seat_number);

    /**
     * @brief get_seats -- Get a vector of seats.
     * @return QVector of Seat instances.
     */
    QVector<std::shared_ptr<Seat>> get_seats();

    /**
     * @brief get_system_mst_user -- Get the MST system user.
     * @return Username as a QString.
     */
    QString get_system_mst_user() const;

    /**
     * @brief get_lightdm_greeter -- Get LightDM greeter name.
     * @return a greeter name as a QString.
     */
    QString get_lightdm_greeter() const;

    /**
     * @brief get_output_directory -- Get the MST configuration
     *      output directory.
     * @return The directory path as a QString.
     */
    QString get_output_directory() const;

private:
    /**
     * @brief system_config -- System configuration.
     */
    std::shared_ptr<DSV> system_config;

    /**
     * @brief seats -- Seats configuration.
     */
    QVector<std::shared_ptr<Seat>> seats;

    /**
     * @brief device_collisions_allowed_p -- if this option is set, then
     * the controller allows device collisions.
     */
    bool device_collisions_allowed_p = false;

    /**
     * @brief empty_devices_allowed_p -- if this option is set, then
     * the controller allows empty device configurations.
     */
    bool empty_devices_allowed_p = false;

    /**
     * @brief missing_components_allowed -- if this option is set to 'true',
     * then the controller will not issue an error if any component is missing
     * from the system.
     */
    bool missing_components_allowed = false;
};

#endif // CONFIGURATION_H
