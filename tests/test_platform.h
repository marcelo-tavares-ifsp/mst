#ifndef TEST_PLATFORM_H
#define TEST_PLATFORM_H

#include <QtTest>
#include <QObject>
#include "test.h"

class Test_platform : public Test
{
    Q_OBJECT
public:
    Test_platform();

private Q_SLOTS:
    void parse_devices_test();
    void fs_rm_test();
    void fs_rm_fail_test();
};

#endif // TEST_PLATFORM_H
