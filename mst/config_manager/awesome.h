#ifndef AWESOME_CONFIG_H
#define AWESOME_CONFIG_H

#include <string>
#include <vector>
#include <sstream>

#include "../configuration/configuration.h"

using namespace std;

class Awesome
{
public:
    Awesome();
    static string make_xephyr_autostart(vector<Seat> seats);
    static string make_xephyr_rules(uint32_t sSize);
    static string make_xephyr_screens(vector<Seat> seats);
};

#endif // AWESOME_CONFIG_H
