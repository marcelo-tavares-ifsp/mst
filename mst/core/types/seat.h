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

    /**
     * @brief get_id -- Get the seat ID.
     * @return Seat ID.
     */
    int get_id() const;

    /**
     * @brief get_mouse -- Get a mouce device linked to
     *     this seat.
     * @return  A mouce device as a QString.
     */
    QString get_mouse() const;

    /**
     * @brief get_keyboard -- Get a keyboard device linked
     *     to this seat.
     * @return A keyboard device as a QString.
     */
    QString get_keyboard() const;

    /**
     * @brief get_usb -- Get a USB port linked to this seat.
     * @return Path to a USB device.
     */
    QString get_usb() const;

    /**
     * @brief get_monitors -- Get all monitor instances linked
     *     to this seat.
     * @return A QVector of monitors.
     */
    QVector<Monitor> get_monitors() const;

    /**
     * @brief get_monitor -- Get @a monitor instance that linked
     *     to this seat.
     * @return A reference to @a monitor.
     */
    Monitor& get_monitor();

    /**
     * @brief add_monitor -- Add @a monitor to this seat.
     * @param monitor -- A monitor to add.
     */
    void add_monitor(Monitor& monitor);

    /**
     * @brief remove_monitor -- Remove @a monitor from the seat.
     * @param monitor -- A monitor to remove.
     */
    void remove_monitor(Monitor& monitor);

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
