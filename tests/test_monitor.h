#ifndef TEST_MONITOR_H
#define TEST_MONITOR_H

#include <QObject>

#include "test.h"

class Test_monitor : public Test
{
    Q_OBJECT
public:
    Test_monitor();

private Q_SLOTS:
    void monitor_from_xrandr_monitor_test();
    void monitor_from_qstring_and_resolutions_test();
    void set_resolution_index_test();
    void set_resolution_test();
    void set_resolution_error_test();
};

#endif // TEST_MONITOR_H
