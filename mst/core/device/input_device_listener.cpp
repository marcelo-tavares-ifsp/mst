#include <unistd.h>
#include <linux/input.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

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

bool Input_device_listener::loop_answer_device(QString device)
{
    struct input_event ie;
    ssize_t bytes;
    int fd;

    QString devpath = Path_manager::get_instance()->get_device_path()
                    + device;

    const char *pDevice = devpath.toStdString().c_str();

    fd = open(pDevice, O_RDWR  | O_NONBLOCK);
    if (fd == -1)
    {
        QString message = "Could not open device: " + device;
        qCritical(input_device_listener_category()) << message;
        return false;
        //throw InputDeviceListener_exception(type, message);
    }

    bytes = device::try_read(fd, &ie);

    if (bytes > 0)
    {
        bool is_pressed;
        if (this->type == DEVICE_TYPE::MOUSE) {
            unsigned char* data = (unsigned char*) &ie;
            is_pressed = (data[0] & 0x1) || (data[0] & 0x2) || (data[0] & 0x4);
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
