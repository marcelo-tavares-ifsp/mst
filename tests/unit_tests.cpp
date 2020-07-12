#include <QString>
#include <QtTest>
//#include <QApplication>

#include <iostream>

#include <vector>
#include <string>

#include "../mst/core/utilites/utilites.h"
#include "../mst/template_manager/template_manager.h"

#include "test_awesome.h"
#include "test_utils.h"
#include "test_template.h"
#include "test_component.h"
#include "test_monitor.h"
#include "test_seat.h"
#include "test_resolution.h"

using namespace std;

int main(int argc, char** argv)
{
    //QApplication app(argc, argv);
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
    Template_manager::get_instance()->set_template_dir("../templates/");

    Test_utils test_utils;
    Test_awesome test_awesome;
    Test_template test_template;
    Test_component test_component;
    Test_monitor   test_monitor;
    Test_seat      test_seat;
    Test_resolution test_resolution;

    return QTest::qExec(&test_utils, argc, argv)
            || QTest::qExec(&test_awesome, argc, argv)
            || QTest::qExec(&test_template, argc, argv)
            || QTest::qExec(&test_component, argc, argv)
            || QTest::qExec(&test_monitor)
            || QTest::qExec(&test_seat)
            || QTest::qExec(&test_resolution);
}
