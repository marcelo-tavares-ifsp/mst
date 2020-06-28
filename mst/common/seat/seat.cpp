#include "seat.h"

Seat::Seat(Monitor& monitor)
    : monitor(monitor)
{

}

bool Seat::intersects(const Seat& other) const
{
    return (this->keyboard == other.keyboard)
            || (this->mouse == other.mouse)
            || (this->usb == other.usb);
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

Monitor Seat::get_monitor() const
{
    return this->monitor;
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
