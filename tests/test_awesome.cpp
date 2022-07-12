#include <QString>
#include <QtTest>

#include <iostream>

#include <vector>
#include <string>
#include <memory>

#include "core/components/awesome.h"
#include "core/types/seat.h"

#include "test_awesome.h"

using namespace std;

Test_awesome::Test_awesome()
{
}

void Test_awesome::make_xephyr_rules() {
    QString result = awesome::make_xephyr_rules(1);
    QFile expected_output_file("./test_awesome_xephyr_rules.lua");
    expected_output_file.open(QIODevice::ReadOnly);
    QString expected_output;
    QTextStream s1(&expected_output_file);
    expected_output.append((s1.readAll()));
    QVERIFY2(result == expected_output, result.toStdString().c_str());
}

