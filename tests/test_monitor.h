#ifndef TEST_MONITOR_H
#define TEST_MONITOR_H

#include <QObject>

class Test_monitor : public QObject
{
    Q_OBJECT
public:
    Test_monitor();

private Q_SLOTS:
    //void monitor_from_xrandr_monitor_test();
    void monitor_from_qstring_and_resolutions_test();
};

#endif // TEST_MONITOR_H
