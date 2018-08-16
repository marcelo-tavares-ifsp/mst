#ifndef DISPLAYMANAGER_H
#define DISPLAYMANAGER_H

#include <string>
#include <stdlib.h>

#include <QLoggingCategory>

Q_DECLARE_LOGGING_CATEGORY(display_manager_category)

using namespace std;



class DisplayManager
{
private:
    string config_path;
public:
    DisplayManager(string config_path);
    void start();
    void add_seat(int seat_number);
    void add_seats(int count);
};

#endif // DISPLAYMANAGER_H