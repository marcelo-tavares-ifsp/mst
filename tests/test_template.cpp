#include <QtTest>

#include "test_template.h"
#include "../mst/core/types/template.h"
#include "../mst/core/template_manager.h"

Test_template::Test_template(QObject *parent) : QObject(parent)
{

}

void Test_template::template_from_string()
{
    QString data = "{{test1}}{{test2}}";
    Template tpl(data);
    QString result = tpl.set("test1", "4").set("test2", "2")
            .substitute();
    QVERIFY2(result == "42", result.toStdString().c_str());
}

void Test_template::template_from_cstring()
{
    QString data = "{{test1}}{{test2}}";
    Template tpl(data.toStdString().c_str());
    QString result = tpl.set("test1", "4").set("test2", "2")
            .substitute();
    QVERIFY2(result == "42", result.toStdString().c_str());
}

void Test_template::template_manager_get_template()
{
    Template_manager* inst = Template_manager::get_instance();
    inst->set_template_dir("./");
    Template tpl = inst->get_template("test_template.txt");
    QString result = tpl.set("number", "1").substitute();
    QVERIFY2(result == "1\n", result.toStdString().c_str());
}
