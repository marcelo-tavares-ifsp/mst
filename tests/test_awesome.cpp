#include <QString>
#include <QtTest>

#include <iostream>

#include <vector>
#include <string>

#include "../mst/config_manager/awesome.h"

#include "test_awesome.h"

using namespace std;

Test_awesome::Test_awesome()
{
}

void Test_awesome::make_xephyr_rules() {
    string result = Awesome::make_xephyr_rules(1);
    qDebug() << QString::fromStdString(result);
}

