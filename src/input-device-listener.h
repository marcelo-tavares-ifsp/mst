#ifndef INPUTDEVICELISTENER_H
#define INPUTDEVICELISTENER_H

#include <QRunnable>
#include <algorithm>
#include <string>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <QObject>

using namespace std;

class Input_device_listener: public QObject, public QRunnable
{
    Q_OBJECT

public:
    enum DEVICE_TYPE {
        KEYBOARD,
        MOUSE
    };

    vector<string> devices;
    DEVICE_TYPE type;

    Input_device_listener(vector<string> devices, DEVICE_TYPE type);

    void run();

signals:
    void device_found(string name, DEVICE_TYPE type);

private:


    string check_keyboards();
    string check_mice();
};

#endif // INPUTDEVICELISTENER_H
