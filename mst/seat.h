#ifndef SEAT_H
#define SEAT_H

#include <string>
#include <stdio.h>

using namespace std;

class Seat
{
public:
    Seat();
    string mouse;
    string interface;
    string keyboard;
    string usb;
    static int width;
    static int height;
};

#endif // SEAT_H
