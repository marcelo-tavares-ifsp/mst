#ifndef SEAT_H
#define SEAT_H

#include <QString>
#include <memory>
#include <ostream>

#include "../monitor/monitor.h"

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

    bool intersects(const Seat& other) const;
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
