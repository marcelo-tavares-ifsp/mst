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
    void load_seats_config_test();
    void get_seat_nullptr();
    void is_valid_zero_seat_test();
    void is_valid_one_unconfigured_seat_test();
    void is_valid_one_configured_seat_test();
};

#endif // TEST_CONFIGURATION_H
