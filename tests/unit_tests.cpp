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
#include "test_configuration.h"
#include "test_monitor.h"
#include "test_seat.h"
#include "test_resolution.h"
#include "test_mstd.h"
#include "test_platform.h"

using namespace std;

int main(int argc, char** argv)
{
    //QApplication app(argc, argv);
    QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
    Template_manager::get_instance()->set_template_dir(SRCDIR "/../templates/");
    QVector<shared_ptr<Test>> tests;
    tests.push_back(make_shared<Test_platform>());
    tests.push_back(make_shared<Test_utils>());
    tests.push_back(make_shared<Test_awesome>());
    tests.push_back(make_shared<Test_template>());
    tests.push_back(make_shared<Test_component>());
    tests.push_back(make_shared<Test_configuration>());
    tests.push_back(make_shared<Test_monitor>());
    tests.push_back(make_shared<Test_seat>());
    tests.push_back(make_shared<Test_resolution>());
    tests.push_back(make_shared<Test_mstd>());

    int rc = 0;
    for (shared_ptr<Test> test : tests) {
        rc |= QTest::qExec(test.get(), argc, argv);
    }

    for (shared_ptr<Test> test : tests) {
        test.reset();
    }

    return rc;
}
