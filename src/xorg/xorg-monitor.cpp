#include "xorg-monitor.h"



XorgMonitor::XorgMonitor(string identifier) : identifier(identifier)
{

}

const string& XorgMonitor::get_identifier() const
{
    return identifier;
}

const string& XorgMonitor::get_interface_name() const
{
    return interface_name;
}

void XorgMonitor::set_dimensions(int width, int height)
{
    this->height = height;
    this->width  = width;
}

void XorgMonitor::set_interface_name(string interface_name)
{
    this->interface_name = interface_name;
}

ostream& operator << (ostream& os, const XorgMonitor& device)
{
    os << "#<xorg-monitor " << device.get_identifier() << " "
       << device.get_interface_name() << " "
       << device.get_width() << "x" << device.get_height() << ">";
    return os;
}

const unsigned int XorgMonitor::get_height() const
{
    return height;
}

const unsigned int XorgMonitor::get_width() const
{
    return width;
}
