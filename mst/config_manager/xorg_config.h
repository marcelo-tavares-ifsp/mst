#ifndef XORG_CONFIG_H
#define XORG_CONFIG_H

#include "configuration/configuration.h"
#include "iostream"
#include <QLoggingCategory>

using namespace std;

class XorgConfig
{
public:
    XorgConfig(vector<Seat> seats);
    vector<Seat> seats;

    friend ostream& operator << (ostream& os, const XorgConfig& config);
};

#endif // XORG_CONFIG_H
