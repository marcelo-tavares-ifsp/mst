#include <QTest>

#include "test_resolution.h"

#include "../mst/common/resolution/resolution.h"

Test_resolution::Test_resolution() : QObject()
{

}

void Test_resolution::parse_string()
{
    QPair<int, int> pair = Resolution::parse_string("640x480");
    QVERIFY( (pair.first == 640) && (pair.second == 480) );
}
