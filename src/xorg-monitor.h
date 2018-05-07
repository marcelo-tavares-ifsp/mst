#ifndef MONITOR_H
#define MONITOR_H

#include <string>

using namespace std;

class XorgMonitor
{
private:
    unsigned int height;
    unsigned int width;
    string interface_name;
    string identifier;

public:
    XorgMonitor(string identifier);
    void set_dimensions(int height, int width);
    void set_interface_name(string interface_name);
    const string& get_identifier() const;
    const string& get_interface_name() const;
    const unsigned int get_width() const;
    const unsigned int get_height() const;

    friend ostream& operator << (ostream& os, const XorgMonitor device);
};

#endif // MONITOR_H
