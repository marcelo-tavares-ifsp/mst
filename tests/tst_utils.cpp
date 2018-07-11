#include <QString>
#include <QtTest>

#include <iostream>

#include <vector>
#include <string>

#include "../src/utils.h"

using namespace std;

class Utils : public QObject
{
    Q_OBJECT

public:
    Utils();

private Q_SLOTS:
    void split_test();
    void replace_all_test();
};

Utils::Utils()
{
}

void Utils::split_test()
{
    string input = "2x4";
    vector<string> result = split(input, 'x');
    string errmsg = "'split' fails to split: '" + result[0] + "x" + result[1] + "'";
    QVERIFY2((result[0] == "2") && (result[1] == "4"),
            errmsg.c_str());
}

void Utils::replace_all_test()
{
    string input = "abc{{d}}";
    string result = replace_all(input, "{{d}}", "e");
    string errmsg = "'replace_all' fails to replace a string: "
            + input + " -> " + result;
    QVERIFY2((result == "abce"), errmsg.c_str());
}

QTEST_APPLESS_MAIN(Utils)

#include "tst_utils.moc"
