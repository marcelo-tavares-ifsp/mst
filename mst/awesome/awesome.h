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

    string get_rules();
    static const string get_raw_version();
    static const vector<int> get_version();

    friend ostream& operator << (ostream& os, const Awesome& config);
};

#endif // AWESOMECONFIG_H
