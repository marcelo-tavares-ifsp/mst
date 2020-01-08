#ifndef XORG_CONFIG_H
#define XORG_CONFIG_H

#include "configuration/configuration.h"
#include "iostream"
#include <QLoggingCategory>

using namespace std;

class Xorg
{
public:
    Xorg(vector<Seat> seats);
    vector<Seat> seats;

    friend ostream& operator << (ostream& os, const Xorg& config);
};

#endif // XORG_CONFIG_H
