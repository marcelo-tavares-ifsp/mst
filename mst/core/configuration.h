#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <memory>
#include <vector>
#include <QLoggingCategory>
#include "core/types/seat.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(configuration_category)

class Configuration
{
public:
    Configuration();

    vector<shared_ptr<Seat>> seats;
};

#endif // CONFIGURATION_H
