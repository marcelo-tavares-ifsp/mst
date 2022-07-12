#ifndef TEST_AWESOME_H
#define TEST_AWESOME_H

#include <QtTest>

#include "test.h"

class Test_awesome : public Test
{
    Q_OBJECT

public:
    Test_awesome();

private Q_SLOTS:
    void make_xephyr_rules();
};

#endif // TEST_AWESOME_H
