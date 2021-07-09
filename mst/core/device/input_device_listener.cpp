#include <unistd.h>
#include <linux/input.h>
#include <linux/input-event-codes.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include "../platform.h"
#include "input_device_listener.h"

Input_device_listener::Input_device_listener(DEVICE_TYPE type,
                                             QVector<QString> devices)
    : Device_listener(type)
{
    this->devices = new QVector<QString>(devices);
}

Input_device_listener::~Input_device_listener()
{
    delete(this->devices);
}

enum {
    VALUE_RELEASED = 0,
    VALUE_PRESSED  = 1,
};

bool Input_device_listener::loop_answer_device(QString device)
{
    struct input_event ie;
    ssize_t bytes;
    int fd;

    QString devpath = platform::INPUT_DEVICES_PATH + device;

    fd = open(devpath.toStdString().c_str(), O_RDWR  | O_NONBLOCK);
    if (fd == -1)
    {
        QString message = "Could not open device: " + device;
        qCritical(input_device_listener_category()) << message;
        return false;
        //throw InputDeviceListener_exception(type, message);
    }

    bytes = device::try_read(fd, &ie);

    if (bytes >= 4)
    {
        bool is_pressed;
        if (this->type == DEVICE_TYPE::MOUSE) {
            switch (ie.type) {
            case EV_KEY:
                is_pressed = ((ie.code >= BTN_LEFT)
                              && (ie.code <= BTN_JOYSTICK));
                break;

            case EV_MSC:
                is_pressed = ((ie.code >= KEY_1) && (ie.code <= KEY_9));
                break;

            default:
                close(fd);
                return false;
            }
        } else {
            is_pressed = device::is_button_pressed(ie);
        }
        if (is_pressed) {
            char name[256] = "Unknown";
            ioctl (fd, EVIOCGNAME (sizeof (name)), name);
            qInfo(input_device_listener_category()) << "Detected: " << name;
            close(fd);
            return true;
        }
    }

    close(fd);
    return false;
}

QString Input_device_listener::poll()
{
    usleep(100);
    for (auto device : *devices) {
        if (loop_answer_device(device)) {
            return device;
        }
    }
    return nullptr;
}
