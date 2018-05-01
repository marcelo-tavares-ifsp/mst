#include <iostream>
#include <string>

#include "xorg-monitor.h"

using namespace std;

XorgMonitor::XorgMonitor(string identifier)
    : identifier(identifier)
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

void XorgMonitor::set_dimensions(int height, int width)
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
    os << "#<xorg-monitor " << device.identifier << " "
       << device.interface_name << " "
       << device.width << "x" << device.height << ">";
    return os;
}
