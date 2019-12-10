#include <string>
#include <vector>
#include <QtTest>

#include "test_utils.h"
#include "../mst/common/utilites/utilites.h"

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

void Test_utils::replace_all_test()
{
    string input = "abc{{d}}";
    string result = replace_all(input, "{{d}}", "e");
    string errmsg = "'replace_all' fails to replace a string: "
            + input + " -> " + result;
    QVERIFY2((result == "abce"), errmsg.c_str());
}
