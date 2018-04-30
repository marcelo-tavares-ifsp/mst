#include <iostream>
#include <string>

#include "xorg-device.h"

using namespace std;

XorgDevice::XorgDevice()
{

}

ostream& operator << (ostream& os, const XorgDevice& device) {
    os << "#<display " << device.identifier << " "
       << device.interface_name << " "
       << device.width << "x" << device.height << ">";
    return os;
}

XorgDevice::operator string() {
    string result
            = "Section \"Device\"\n"
            + "  Identifier: " + this->identifier + "\n"
            + "  Option"

}
