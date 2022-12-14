#include <QtTest>
#include "test_mstd.h"

Test_mstd::Test_mstd() : Test()
{

}

bool Test_mstd::run_guile_test(QString file_name)
{
    QString command
            = "guile -L "  + QString(SRCDIR) + "/modules"
            + " --no-auto-compile -s '" + file_name + "'";
    return system(command.toStdString().c_str()) == 0;
}

void Test_mstd::system_test()
{
    QVERIFY(run_guile_test("mstd-system.scm"));
}

void Test_mstd::config_test() {
    QVERIFY(run_guile_test("mstd-config.scm"));
}

void Test_mstd::dm_test()
{
  QVERIFY(run_guile_test("mstd-dm.scm"));
}
