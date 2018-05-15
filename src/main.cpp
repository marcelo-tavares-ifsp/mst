#include "mainwindow.h"
#include "xorg/xorg-config.h"
#include "xorg/xorg-monitor.h"
#include "awesome/awesome-config.h"
#include "awesome/awesome-device.h"
#include "controller_mst.h"

#include <QApplication>
#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

void create_rc_lua(AwesomeConfig *config);
void create_xorg(XorgConfig *config);

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    /*XorgMonitor *monitor_1 = new XorgMonitor("monitor0");
    XorgMonitor *monitor_2 = new XorgMonitor("monitor1");
    XorgConfig *config_1 = new XorgConfig();

    monitor_1->set_dimensions(1920, 1080);
    monitor_1->set_interface_name("Monitor-DVI-1");

    monitor_2->set_dimensions(1920, 1080);
    monitor_2->set_interface_name("Monitor-VGA-1");

    config_1->add_monitor(*monitor_1);
    config_1->add_monitor(*monitor_2);

    AwesomeDevice *device_1 = new AwesomeDevice(1);
    device_1->set_dimensions(1920, 1080);

    AwesomeDevice *device_2 = new AwesomeDevice(2);
    device_2->set_dimensions(1920, 1080);

    AwesomeConfig *config_2 = new AwesomeConfig();
    config_2->add_devices(*device_1);
    config_2->add_devices(*device_2);*/

    Controller *controller = new Controller();
    AwesomeConfig *config_2 = controller->create_rclua();
    XorgConfig *config_1 = controller->create_xorg();

    create_rc_lua(config_2);
    create_xorg(config_1);

    return a.exec();
}

void create_rc_lua(AwesomeConfig *config)
{
    fstream rclua_pattern;
    rclua_pattern.open("D:\\Programming\\Git\\mst\\src\\mst_files\\rc.lua.pattern", ios::in);
    fstream rclua;
    rclua.open("D:\\Programming\\Git\\mst\\src\\test_files\\home\\multiseat\\config\\awesome\\rc.lua", ios::out);

    string str;

    while(getline(rclua_pattern, str))
    {
        if(str == "-- $MST_AUTOSTART$")
        {
            rclua << *config;
        }
        else if(str == "-- $MST_AWFUL_RULES$")
        {
            rclua << config->get_rules();
        }
        else
        {
            rclua << str << endl;
        }
    }
    rclua.close();
    rclua_pattern.close();
}

void create_xorg(XorgConfig *config)
{
    fstream xorg;
    xorg.open("D:\\Programming\\Git\\mst\\src\\test_files\\etc\\X11\\xorg.conf", ios::out);

    xorg << *config;

    xorg.close();
}
