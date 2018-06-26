#ifndef XORGCONFIG_H
#define XORGCONFIG_H

#include <vector>
#include <string>
#include <iostream>
#include <functional>
#include <numeric>

#include "seat.h"

using namespace std;



class XorgConfig
{
public:
    XorgConfig(vector<Seat> seats);
    vector<Seat> seats;

    friend ostream& operator << (ostream& os, const XorgConfig& config);
};

#endif // XORGCONFIG_H
