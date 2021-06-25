#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <memory>
#include <vector>
#include <QLoggingCategory>
#include <core/dsv_parser/dsv.h>
#include "core/types/seat.h"

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

class Configuration
{
public:
    Configuration();

    void load(QString system_config_file);

    void set_debug_allow_device_collisions(bool value);
    void set_debug_allow_empty_devices(bool value);

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
     * @param idx -- A seat index.
     * @return A shared pointer to a seat.
     */
    std::shared_ptr<Seat> get_seat(int32_t idx);

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
     * @brief get_output_directory -- Get the MST configuration
     *      output directory.
     * @return The directory path as a QString.
     */
    QString get_output_directory() const;

private:
    std::shared_ptr<DSV> system_config;
    QVector<std::shared_ptr<Seat>> seats;

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

#endif // CONFIGURATION_H
