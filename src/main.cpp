#include "mainwindow.h"
#include "xorg/xorg-config.h"
#include "xorg/xorg-monitor.h"
#include "awesome/awesome-config.h"
#include "awesome/awesome-device.h"
#include <fstream>

#include <QApplication>
#include <iostream>
#include <QFile>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

void write_config(XorgConfig &config, string prev_data, string file);

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    XorgMonitor *monitor_1 = new XorgMonitor("monitor0");
    XorgMonitor *monitor_2 = new XorgMonitor("monitor1");
    XorgConfig *config_1 = new XorgConfig();

    monitor_1->set_dimensions(1920, 1080);
    monitor_1->set_interface_name("Monitor-DVI-1");

    monitor_2->set_dimensions(1920, 1080);
    monitor_2->set_interface_name("Monitor-VGA-1");

    config_1->add_monitor(*monitor_1);
    config_1->add_monitor(*monitor_2);

    cout << *config_1;

    cout << "/////////////////////////////////////////////////////////////////////////////\n\n";

    AwesomeDevice *device_1 = new AwesomeDevice(1);
    device_1->set_dimensions(1920, 1080);

    AwesomeDevice *device_2 = new AwesomeDevice(2);
    device_2->set_dimensions(1920, 1080);

    AwesomeConfig *config_2 = new AwesomeConfig();
    config_2->add_devices(*device_1);
    config_2->add_devices(*device_2);

    cout << *config_2 << endl;
    cout << config_2->get_rules();

    return a.exec();
}
