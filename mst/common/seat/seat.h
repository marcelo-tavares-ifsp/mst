#ifndef SEAT_H
#define SEAT_H

#include <QString>

#include "common/monitor/monitor.h"

class Seat
{
public:
    Seat(Monitor& monitor);
    QString get_mouse() const;
    QString get_keyboard() const;
    QString get_usb() const;
    Monitor get_monitor() const;

    void set_mouse(QString mouse);
    void set_keyboard(QString keyboard);
    void set_usb(QString usb);

    bool intersects(const Seat& other) const;

private:
    QString mouse;
    QString keyboard;
    QString usb;
    Monitor monitor;
};

#endif // SEAT_H
