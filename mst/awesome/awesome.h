#ifndef AWESOMECONFIG_H
#define AWESOMECONFIG_H

#include <vector>
#include <string>
#include <iostream>

#include <QLoggingCategory>

#include "seat.h"

Q_DECLARE_LOGGING_CATEGORY(awesome_category)

using namespace std;



class Awesome
{
public:
    Awesome(vector<Seat> seats);
    vector<Seat> seats;

    string make_rules();
    string make_autostart();
    static const string get_raw_version();
    static const vector<int> get_version();
};

#endif // AWESOMECONFIG_H
