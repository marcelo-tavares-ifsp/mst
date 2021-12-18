#include <string>
#include <vector>
#include <QtTest>

#include "test_utils.h"
#include "core/utilites/utilites.h"

using namespace std;

Test_utils::Test_utils()
{
}

void Test_utils::split_test()
{
    string input = "2x4";
    vector<string> result = split(input, 'x');
    string errmsg = "'split' fails to split: '" + result[0] + "x" + result[1] + "'";
    QVERIFY2((result[0] == "2") && (result[1] == "4"),
            errmsg.c_str());
}
