#ifndef AWESOMECONFIG_H
#define AWESOMECONFIG_H

#include <vector>
#include <string>
#include <iostream>

#include "seat.h"

using namespace std;



class Awesome
{
public:
    Awesome(vector<Seat> seats);
    vector<Seat> seats;

    string get_rules();

    friend ostream& operator << (ostream& os, const Awesome& config);
};

#endif // AWESOMECONFIG_H
