#ifndef TEST_COMPONENT_H
#define TEST_COMPONENT_H

#include <QObject>

#include "test.h"

class Test_component : public Test
{
    Q_OBJECT
public:
    Test_component();

private Q_SLOTS:
    void configuration_test();
};

#endif // TEST_COMPONENT_H
