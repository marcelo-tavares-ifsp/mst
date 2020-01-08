#ifndef DISPLAY_MANAGER_H
#define DISPLAY_MANAGER_H

#include <string>
#include <stdlib.h>
#include "command_manager/commandmanager.h"
#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(display_manager_category)

class Display_manager
{
public:
    Display_manager();
    void start();
    void add_seat(int seat_number);
    void add_seats(int count);
};

#endif // DISPLAY_MANAGER_H
