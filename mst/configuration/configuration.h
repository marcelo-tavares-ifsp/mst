#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <vector>
#include <QLoggingCategory>
#include <memory>
#include "../core/monitor/monitor.h"
#include "../core/seat/seat.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

class Configuration
{
public:
    Configuration();

    vector<shared_ptr<Seat>> seats;
};

#endif // CONFIGURATION_H
