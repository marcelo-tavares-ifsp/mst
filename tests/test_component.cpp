#include <QtTest>
#include <QString>

#include "../mst/core/platform.h"
#include "test_component.h"
#include "../mst/core/types/resolution.h"
#include "../mst/core/types/monitor.h"
#include "../mst/core/component.h"
#include "../mst/core/types/seat.h"

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
