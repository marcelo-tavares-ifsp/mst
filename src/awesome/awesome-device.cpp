#include "awesome-device.h"



AwesomeDevice::AwesomeDevice(unsigned int identifier) : identifier(identifier)
{
    mouse = set_mouse();
    keyboard = set_keyboard();
}

void AwesomeDevice::set_dimensions(int width, int height)
{
    this->height = height;
    this->width  = width;
}

string AwesomeDevice::set_mouse()
{
    return "/dev/input/by-path/pci-0000:00:12.0-usb-0:2:1.0-event-mouse";
}

string AwesomeDevice::set_keyboard()
{
    return "evdev,,device=/dev/input/by-path/pci-0000:00:12.0-usb-0:1:1.0-event-kbd";
}

const unsigned int AwesomeDevice::get_height() const
{
    return height;
}

const unsigned int AwesomeDevice::get_width() const
{
    return width;
}

const string AwesomeDevice::get_mouse() const
{
    return mouse;
}

const string AwesomeDevice::get_keyboard() const
{
    return keyboard;
}

const unsigned int AwesomeDevice::get_identifier() const
{
    return identifier;
}

ostream& operator << (ostream& os, const AwesomeDevice& devices)
{
    os << "#<awesome-device " << devices.get_identifier() << " "
       << devices.get_mouse() << " " << devices.get_keyboard() << " "
       << devices.get_width() << "x" << devices.get_height() << ">";
    return os;
}
