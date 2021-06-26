#ifndef TEST_UTILS_H
#define TEST_UTILS_H

#include <QTest>

#include "test.h"

class Test_utils : public Test
{
    Q_OBJECT

public:
    Test_utils();

private Q_SLOTS:
    void split_test();
   // void replace_all_test();
};

#endif // TEST_UTILS_H
