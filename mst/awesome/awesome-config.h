#ifndef AWESOMECONFIG_H
#define AWESOMECONFIG_H

#include <vector>
#include <string>
#include <iostream>

#include "seat.h"

using namespace std;



class AwesomeConfig
{
public:
    AwesomeConfig(vector<Seat> seats);
    vector<Seat> seats;

    string get_rules();

    friend ostream& operator << (ostream& os, const AwesomeConfig& config);
};

#endif // AWESOMECONFIG_H
