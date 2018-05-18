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

void write_rc_lua(AwesomeConfig *config);
void write_xorg(XorgConfig *config);

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    Controller *controller = new Controller();

    // controller->make_mst();
    controller->enable_mst();
    // controller->disable_mst();

    return a.exec();
}
