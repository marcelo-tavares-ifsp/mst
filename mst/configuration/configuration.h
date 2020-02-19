#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <vector>
#include <QLoggingCategory>

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

struct Resolution
{
    int width;
    int height;
};

struct Seat
{
    QString interface;
    string mouse;
    string keyboard;
    string usb;
    struct Resolution resolution;
};

class Configuration
{
public:
    Configuration();

    vector<Seat> seats;
};

#endif // CONFIGURATION_H
