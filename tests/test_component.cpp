#include <QtTest>
#include <QString>

#include "test_component.h"
#include "core/platform.h"
#include "core/types/resolution.h"
#include "core/types/monitor.h"
#include "core/types/seat.h"
#include "core/component.h"

Test_component::Test_component()
{

}

void Test_component::configuration_test()
{
    Configuration sysconf;
    Component_configuration config(sysconf);
    config.add("test.txt", ".", Template("{{test}}"));
    QString result = config.get_template("test.txt")
            .set("test", "42")
            .substitute();

    QVERIFY2(result == "42", result.toStdString().c_str());
}
