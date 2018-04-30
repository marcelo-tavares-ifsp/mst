#ifndef MONITOR_H
#define MONITOR_H

#include <string>

using namespace std;

class XorgDevice
{
private:
    unsigned int height;
    unsigned int width;
    string interface_name;
    string identifier;
public:
    XorgDevice();

    operator string();
    friend ostream& operator << (ostream& os, const XorgDevice& device);
};

#endif // MONITOR_H
