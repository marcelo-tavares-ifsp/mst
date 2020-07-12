#include <QTest>
#include <sstream>
#include <iostream>
#include "test_resolution.h"

#include "../mst/core/types/resolution.h"

Test_resolution::Test_resolution() : QObject()
{

}

void Test_resolution::parse_string()
{
    QPair<int, int> pair = Resolution::parse_string("640x480");
    QVERIFY( (pair.first == 640) && (pair.second == 480) );
}

void Test_resolution::to_string()
{
    Resolution res("640x480");
    std:stringstream ss;
    ss << res;
    QVERIFY( ss.str() == "#<Resolution 640x480>" );
}
