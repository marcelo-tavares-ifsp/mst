#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <vector>
#include <QLoggingCategory>
#include "common/seat/seat.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

class Configuration
{
public:
    Configuration();

    vector<Seat> seats;
};

#endif // CONFIGURATION_H
