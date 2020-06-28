#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <vector>
#include <QLoggingCategory>
#include "common/resolution/resolution.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

struct Seat
{
    QString interface;
    QString mouse;
    QString keyboard;
    QString usb;
    Resolution resolution;
};

class Configuration
{
public:
    Configuration();

    vector<Seat> seats;
};

#endif // CONFIGURATION_H
