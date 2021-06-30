#include <QtTest>

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
