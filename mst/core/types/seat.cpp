#include <QLoggingCategory>
#include <memory>
#include <ostream>

#include "seat.h"

Q_LOGGING_CATEGORY(seat_category, "mst.core.types.seat")

std::ostream& operator<< (std::ostream& os, Seat& seat) {
    os << "#<Seat " << seat.get_monitor().get_interface().toStdString()
       << " keyboard: " << seat.get_keyboard().toStdString()
       << " mouse: "    << seat.get_mouse().toStdString()
       << " usb: "    << seat.get_usb().toStdString()
       << ">";
    return os;
}

Seat::Seat(int id)
    : id(id)
{

}

bool Seat::intersects(const Seat& other) const
{
    return (this->keyboard == other.keyboard)
            || (this->mouse == other.mouse)
            || (this->usb == other.usb);
}

bool Seat::intersects(const shared_ptr<Seat> other) const
{
    return (this->keyboard == other->keyboard)
            || (this->mouse == other->mouse)
            || (this->usb == other->usb);
}

int Seat::get_id() const
{
    return this->id;
}

QString Seat::get_keyboard() const
{
    return this->keyboard;
}

QString Seat::get_mouse() const
{
    return this->mouse;
}

QString Seat::get_usb() const
{
    return this->usb;
}

void Seat::add_monitor(Monitor &monitor)
{
    this->monitors.push_back(monitor);
}

// TODO: Needed only for compatibility.
Monitor &Seat::get_monitor()
{
    return this->monitors[0];
}

QVector<Monitor> Seat::get_monitors() const
{
    return this->monitors;
}

void Seat::set_keyboard(QString keyboard)
{
    this->keyboard = keyboard;
}

void Seat::set_mouse(QString mouse)
{
    this->mouse = mouse;
}

void Seat::set_usb(QString usb)
{
    this->usb = usb;
}

bool Seat::is_configured() const
{
    QString msg = "";
    bool result = true;

    if (get_keyboard() == "") {
        msg += "KEYBOARD, ";
        result = true;
    }
    if (get_mouse() == "") {
        msg += "MOUSE, ";
        result = true;
    }
    if (get_usb() == "") {
        msg += "USB, ";
        result = true;
    }

    qInfo(seat_category()) << msg << "not configured";

    return result;
}
