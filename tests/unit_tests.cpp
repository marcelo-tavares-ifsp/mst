#include <QString>
#include <QtTest>
//#include <QApplication>

#include <iostream>

#include <vector>
#include <string>

#include "../mst/core/utilites/utilites.h"
#include "../mst/core/template_manager.h"

#include "test_awesome.h"
#include "test_utils.h"
#include "test_template.h"
#include "test_component.h"
#include "test_monitor.h"
#include "test_seat.h"
#include "test_resolution.h"
#include "test_mstd.h"

using namespace std;

int main(int argc, char** argv)
{
    //QApplication app(argc, argv);
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
    Template_manager::get_instance()->set_template_dir(SRCDIR "/../templates/");

    Test_utils test_utils;
    Test_awesome test_awesome;
    Test_template test_template;
    Test_component test_component;
    Test_monitor   test_monitor;
    Test_seat      test_seat;
    Test_resolution test_resolution;
    Test_mstd test_mstd;

    return QTest::qExec(&test_utils, argc, argv)
            || QTest::qExec(&test_awesome, argc, argv)
            || QTest::qExec(&test_template, argc, argv)
            || QTest::qExec(&test_component, argc, argv)
            || QTest::qExec(&test_monitor, argc, argv)
            || QTest::qExec(&test_seat, argc, argv)
            || QTest::qExec(&test_resolution, argc, argv)
            || QTest::qExec(&test_mstd, argc, argv);
}
