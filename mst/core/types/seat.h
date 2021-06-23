#ifndef SEAT_H
#define SEAT_H

#include <QLoggingCategory>
#include <QString>
#include <memory>
#include <ostream>

#include "monitor.h"

Q_DECLARE_LOGGING_CATEGORY(seat_category)

/**
 * @brief The Seat class -- Describes a MST seat configuration.
 */
class Seat
{
public:
    Seat(int id);
    int get_id() const;
    QString get_mouse() const;
    QString get_keyboard() const;
    QString get_usb() const;
    QVector<Monitor> get_monitors() const;
    Monitor& get_monitor();

    /**
     * @brief add_monitor -- Add a monitor to this seat.
     * @param monitor -- A monitor to add.
     */
    void add_monitor(Monitor& monitor);

    /**
     * @brief set_mouse -- Attach a mouse to this seat.
     * @param mouse -- A mouse to attach.
     */
    void set_mouse(QString mouse);

    /**
     * @brief set_keyboard -- Attach a keyboard to this seat.
     * @param keyboard -- A keyboard to attach.
     */
    void set_keyboard(QString keyboard);

    /**
     * @brief set_usb -- Attach a USB port to this seat.
     * @param usb -- A USB port to attach.
     */
    void set_usb(QString usb);

    /**
     * @brief is_configured -- Predicate.  Checks if the seat is configured.
     * @return true if it is, false otherwise.
     */
    bool is_configured() const;

    /**
     * @brief intersects -- Check if the seat have common devices with other
     *     seat.
     * @param other -- other seat reference to check.
     * @return true if seats have common devices, false otherwise.
     */
    bool intersects(const Seat& other) const;

    /**
     * @brief intersects -- Check if the seat have common devices with other
     *     seat.
     * @param other -- other seat shared pointer to check.
     * @return true if seats have common devices, false otherwise.
     */
    bool intersects(const std::shared_ptr<Seat> other) const;

    /**
     * @brief reset_devices -- Reset mouse, keyboard and USB devices
     *     to an empty string.
     */
    void reset_devices();

    friend std::ostream& operator<< (std::ostream& os, Seat& seat);

private:
    int id;
    QString mouse;
    QString keyboard;
    QString usb;
    QVector<Monitor> monitors;
};

#endif // SEAT_H
