#ifndef SEAT_H
#define SEAT_H

#include <QLoggingCategory>
#include <QString>
#include <memory>
#include <ostream>

#include "monitor.h"

Q_DECLARE_LOGGING_CATEGORY(seat_category)

/**
 * @brief The Seat class -- describes a MST seat configuration.
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
    void add_monitor(Monitor& monitor);
    void set_mouse(QString mouse);
    void set_keyboard(QString keyboard);
    void set_usb(QString usb);
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
    bool intersects(const shared_ptr<Seat> other) const;

    friend std::ostream& operator<< (std::ostream& os, const Seat& seat);

private:
    int id;
    QString mouse;
    QString keyboard;
    QString usb;
    QVector<Monitor> monitors;
};

#endif // SEAT_H
