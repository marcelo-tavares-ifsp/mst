#ifndef INPUTDEVICELISTENER_H
#define INPUTDEVICELISTENER_H

#include <QRunnable>
#include <QString>
#include <algorithm>
#include <string>
#include <linux/input.h>
#include <iostream>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <QObject>

using namespace std;



static const string FULLPATH = "/dev/input/by-path/";

class Input_device_listener: public QObject, public QRunnable
{
    Q_OBJECT


public:
    enum DEVICE_TYPE {
        KEYBOARD,
        MOUSE
    };

    vector<string> *devices;
    DEVICE_TYPE type;

    Input_device_listener(vector<string> devices, DEVICE_TYPE type);

    void run();

signals:
    void device_found(QString, int);

private:

    string check_keybd();
    string check_mice();
};
//Q_DECLARE_METATYPE(Input_device_listener)

#endif // INPUTDEVICELISTENER_H