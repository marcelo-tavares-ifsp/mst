#include <QtTest>
#include <QString>

#include "test_component.h"
#include "../mst/common/resolution/resolution.h"
#include "../mst/common/monitor/monitor.h"
#include "../mst/components/component.h"
#include "../mst/common/seat/seat.h"

Test_component::Test_component()
{

}

void Test_component::configuration_test()
{
    Component_configuration config;
    config.add("test.txt", ".", Template("{{test}}"));
    QString result = config.get_template("test.txt")
            .set("test", "42")
            .substitute();

    QVERIFY2(result == "42", result.toStdString().c_str());
}
