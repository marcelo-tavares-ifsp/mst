#ifndef INPUTDIVECELISTENER_H
#define INPUTDIVECELISTENER_H

#include <string>
#include <vector>
#include <unistd.h>
#include <linux/input.h>
#include <libudev.h>
#include <QObject>
#include <QRunnable>
#include <QString>
#include <algorithm>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <QLoggingCategory>
#include "path_manager/pathmanager.h"
#include "core/utilites/utilites.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(input_device_listener_category)

enum DEVICE_TYPE {
    KEYBOARD,
    MOUSE,
    USB
};

class InputDeviceListener_exception : public runtime_error
{
  public:
    InputDeviceListener_exception(DEVICE_TYPE type, string& what)
        : runtime_error(what),
          type(type)
    {
        /* Do nothing */
    }
    DEVICE_TYPE get_type()
    {
        return type;
    }
  private:
    DEVICE_TYPE type;
};

class InputDeviceListener: public QObject, public QRunnable
{
    Q_OBJECT

public:
    vector<string> *devices;
    DEVICE_TYPE type;

    InputDeviceListener(DEVICE_TYPE type, vector<string> devices);

    void run();

signals:
    void device_found(QString, DEVICE_TYPE);
    void work_done();

public slots:
    void cancel();

private:
    bool is_running;
    string* check_device();
    string* check_usb();
    bool loop_answer_device(string device);
};

Q_DECLARE_METATYPE(DEVICE_TYPE)

#endif // INPUTDIVECELISTENER_H
