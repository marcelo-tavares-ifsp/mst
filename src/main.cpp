#include "mainwindow.h"
#include "xorg-config.h"
#include "xorg-monitor.h"

#include <QApplication>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    XorgMonitor *monitor_1 = new XorgMonitor("monitor0");
    XorgMonitor *monitor_2 = new XorgMonitor("monitor1");
    XorgConfig *config_1 = new XorgConfig();

    monitor_1->set_dimensions(1080, 1600);
    monitor_1->set_interface_name("Monitor-DVI-1");

    monitor_2->set_dimensions(1080, 1600);
    monitor_2->set_interface_name("Monitor-VGA-1");

    config_1->add_monitor(*monitor_1);
    config_1->add_monitor(*monitor_2);

    cout << *config_1;

    return a.exec();
}
