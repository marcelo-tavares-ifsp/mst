#ifndef TEST_RESOLUTION_H
#define TEST_RESOLUTION_H

#include <QObject>

#include "test.h"

class Test_resolution : public Test
{
    Q_OBJECT
public:
    Test_resolution();

private Q_SLOTS:
    void parse_string();
    void parse_string_error();
    void to_string();

};

#endif // TEST_RESOLUTION_H
