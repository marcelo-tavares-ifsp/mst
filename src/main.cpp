#include <QApplication>
#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <regex>
//#include <QMetaType>

#include "mainwindow.h"
#include "xorg/xorg-config.h"
#include "xorg/xorg-monitor.h"
#include "awesome/awesome-config.h"
#include "awesome/awesome-device.h"
#include "controller_mst.h"
#include "settings-mst.h"
#include "input-device-listener.h"

using namespace std;



int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    //qRegisterMetaType<Input_device_listener>("Input_device_listener");
    Controller *controller = new Controller(2);

    controller->enable_mst();
    controller->disable_mst();

    return a.exec();


//    while (1)
//    {
//        Settings_mst::loop_answer(list_mice);
//        Settings_mst::loop_answer(list_keybs);
//    }

    cout << "the end!" << endl;

}
