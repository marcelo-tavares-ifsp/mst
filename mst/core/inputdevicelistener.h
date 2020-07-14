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
#include "core/path_manager.h"
#include "core/utilites/utilites.h"

using namespace std;

Q_DECLARE_LOGGING_CATEGORY(input_device_listener_category)

enum DEVICE_TYPE {
    KEYBOARD,
    MOUSE,
    USB
};

class Device_listener_exception : public runtime_error
{
  public:
    Device_listener_exception(DEVICE_TYPE type, QString& what)
        : runtime_error(what.toStdString()),
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

class Device_listener: public QObject, public QRunnable
{
    Q_OBJECT

public:
    QVector<QString> *devices;
    DEVICE_TYPE type;

    Device_listener(DEVICE_TYPE type, QVector<QString> devices);

    void run();

signals:
    void device_found(QString, DEVICE_TYPE);
    void work_done();

public slots:
    void cancel();

private:
    bool is_running;
    QString check_device();
    QString check_usb();
    bool loop_answer_device(QString device);
};

Q_DECLARE_METATYPE(DEVICE_TYPE)

#endif // INPUTDIVECELISTENER_H
