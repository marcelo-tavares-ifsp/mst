#include <QtTest>
#include <sstream>

#include "test_monitor.h"

#include "core/types/xrandr_monitor.h"
#include "core/types/monitor.h"
#include "core/types/resolution.h"

Test_monitor::Test_monitor() : Test()
{

}

void Test_monitor::monitor_from_qstring_and_resolutions_test()
{
    QVector<Resolution> resolutions = { Resolution("640x480") };
    Monitor monitor("VGA-1", resolutions);
    QVERIFY2(monitor.get_interface() == "VGA-1",
             monitor.get_interface().toStdString().c_str());

    QString result = monitor.get_current_resolution().to_string();
    QVERIFY2(result == "640x480",
             result.toStdString().c_str());
}

void Test_monitor::monitor_from_xrandr_monitor_test()
{
    XRandr_monitor xrandr_monitor;
    xrandr_monitor.interface = "VGA-1";
    xrandr_monitor.resolutions.push_back("640x480");
    Monitor monitor(xrandr_monitor);

    QVERIFY2(monitor.get_interface() == "VGA-1",
             monitor.get_interface().toStdString().c_str());

    QString result = monitor.get_current_resolution().to_string();
    QVERIFY2(result == "640x480",
             result.toStdString().c_str());
}

void Test_monitor::set_resolution_index_test()
{
    QVector<Resolution> resolutions = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    Monitor monitor("VGA-1", resolutions);
    monitor.set_resolution(1);
    QString result = monitor.get_current_resolution().to_string();
    QVERIFY2(result == "640x480",
             result.toStdString().c_str());
}

void Test_monitor::set_resolution_test()
{
    QVector<Resolution> resolutions = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    Monitor monitor("VGA-1", resolutions);
    monitor.set_resolution(Resolution("640x480"));
    QString result = monitor.get_current_resolution().to_string();
    QVERIFY2(result == "640x480",
             result.toStdString().c_str());
}

void Test_monitor::set_resolution_error_test()
{
    QVector<Resolution> resolutions = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    Monitor monitor("VGA-1", resolutions);
    QVERIFY_EXCEPTION_THROWN(monitor.set_resolution(Resolution("1280x1024")),
                             Monitor_error);
}

void Test_monitor::equality_test()
{
    QVector<Resolution> resolutions = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    Monitor monitor1("VGA-1", resolutions);
    Monitor monitor2("VGA-1", resolutions);
    QVERIFY2(monitor1 == monitor2,
             "Monitor 1 not equal to Monitor 2");
}

void Test_monitor::inequality_in_resolutions_test()
{
    QVector<Resolution> resolutions1 = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    QVector<Resolution> resolutions2 = {
        Resolution("1280x1024"),
        Resolution("1024x768")
    };
    Monitor monitor1("VGA-1", resolutions1);
    Monitor monitor2("VGA-1", resolutions2);
    QVERIFY2(! (monitor1 == monitor2),
             "Monitor 1 equal to Monitor 2");
}

void Test_monitor::inequality_in_interfaces_test()
{
    QVector<Resolution> resolutions = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    Monitor monitor1("VGA-1", resolutions);
    Monitor monitor2("VGA-2", resolutions);
    QVERIFY2(! (monitor1 == monitor2),
             "Monitor 1 equal to Monitor 2");
}

void Test_monitor::monitor_output_format_test()
{
    QVector<Resolution> resolutions = {
        Resolution("1024x768"),
        Resolution("640x480")
    };
    Monitor monitor("VGA-1", resolutions);
    std::stringstream os;
    os << monitor;
    QVERIFY2(os.str() == "#<Monitor VGA-1>",
             os.str().c_str());
}
