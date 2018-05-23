#include "mainwindow.h"
#include "xorg/xorg-config.h"
#include "xorg/xorg-monitor.h"
#include "awesome/awesome-config.h"
#include "awesome/awesome-device.h"
#include "controller_mst.h"
#include "settings-mst.h"

#include <QApplication>
#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <regex>

using namespace std;

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    Controller *controller = new Controller();

    controller->enable_mst();
    // controller->disable_mst();

    for (Xrandr_monitor mon : Settings_mst::parse_xrandr())
    {
        cout << mon.interface << endl;
        for (auto res : mon.resolutions)
        {
            cout << res << endl;;
        }
    }

    return a.exec();
}
