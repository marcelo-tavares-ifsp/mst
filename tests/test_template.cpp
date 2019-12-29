#include <QtTest>

#include "test_template.h"
#include "../mst/template_manager/template.h"
#include "../mst/template_manager/template_manager.h"

Test_template::Test_template(QObject *parent) : QObject(parent)
{

}

void Test_template::template_from_string()
{
    string data = "{{test1}}{{test2}}";
    Template tpl(data);
    string result = tpl.set("test1", "4").set("test2", "2")
            .substitute();
    QVERIFY2(result == "42", result.c_str());
}

void Test_template::template_from_cstring()
{
    string data = "{{test1}}{{test2}}";
    Template tpl(data.c_str());
    string result = tpl.set("test1", "4").set("test2", "2")
            .substitute();
    QVERIFY2(result == "42", result.c_str());
}

void Test_template::template_manager_get_template()
{
    Template_manager* inst = Template_manager::get_instance();
    inst->set_template_dir("./");
    Template tpl = inst->get_template("test_template.txt");
    string result = tpl.set("number", "1").substitute();
    QVERIFY2(result == "1\n", result.c_str());
}
