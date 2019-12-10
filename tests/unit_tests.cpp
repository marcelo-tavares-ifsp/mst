#include <QString>
#include <QtTest>
//#include <QApplication>

#include <iostream>

#include <vector>
#include <string>

#include "../mst/common/utilites/utilites.h"

#include "test_awesome.h"
#include "test_utils.h"



using namespace std;

int main(int argc, char** argv)
{
    //QApplication app(argc, argv);
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

    Test_utils test_utils;
    Test_awesome test_awesome;

    return QTest::qExec(&test_utils, argc, argv)
            || QTest::qExec(&test_awesome, argc, argv);
}
