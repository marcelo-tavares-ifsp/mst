#ifndef AWESOMEDEVICE_H
#define AWESOMEDEVICE_H

#include <string>
#include <iostream>

using namespace std;



class AwesomeDevice
{
private:
    unsigned int identifier;
    string mouse;
    string keyboard;
    unsigned int height;
    unsigned int width;
    string set_mouse();
    string set_keyboard();

public:
    AwesomeDevice(unsigned int identifier);
    void set_dimensions(int width, int height);
    const unsigned int get_width() const;
    const unsigned int get_height() const;
    const string get_mouse() const;
    const string get_keyboard() const;
    const unsigned int get_identifier() const;

    friend ostream& operator << (ostream& os, const AwesomeDevice devices);
};

#endif // AWESOMEDEVICE_H
