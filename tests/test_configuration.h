#ifndef TEST_CONFIGURATION_H
#define TEST_CONFIGURATION_H

#include "test.h"

#include <QObject>

class Test_configuration : public Test
{
    Q_OBJECT
public:
    explicit Test_configuration();

private Q_SLOTS:
    void load_non_existing_config_test();
};

#endif // TEST_CONFIGURATION_H
