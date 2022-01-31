#include <unistd.h>
#include <linux/input.h>
#include <libudev.h>
#include <algorithm>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include <QLoggingCategory>

#include "device.h"

using namespace std;
using namespace device;

Q_LOGGING_CATEGORY(device_category, "mst.core.device")

/**
 * @brief try_read -- try to read data from a mouse in a loop.
 * @param fd -- mouse file descriptor.
 * @param ie -- input event.
 * @return -1 if there's no data to read, amount of data read otherwise.
 */
ssize_t device::try_read(int fd, struct input_event* ie)
{
    static uint32_t MAX_COUNT = 50;
    ssize_t bytes = -1;
    for (uint32_t count = 0; count < MAX_COUNT; ++count) {
        bytes = read(fd, (void *) ie, sizeof(struct input_event));
        if (bytes >= (ssize_t) sizeof(struct input_event))
            break;
        usleep(100);
    }
    return bytes;
}

bool device::is_button_pressed(struct input_event &e)
{
    qInfo(device_category()) << "e.type: "
        << e.type << "; e.code: " << e.code << "; e.value: " << e.value;
    return (e.type == EV_MSC) && (e.code == 4);
}


//// USB device scanner.

USB_device_scanner::USB_device_scanner()
{
    udev = udev_new();
    if (! udev) {
        QString msg = "udev_new() returned NULL";
        qCritical(device_category()) << msg;
        throw Device_exception(msg);
    }

    udev_monitor = udev_monitor_new_from_netlink(udev, "udev");
    if (! udev_monitor) {
        QString msg =  "udev_monitor_new_from_netlink() retruned NULL";
        qCritical(device_category()) << msg;
        throw Device_exception(msg);
    }

    int ret = udev_monitor_filter_add_match_subsystem_devtype(udev_monitor,
                                                              "usb",
                                                              "usb_device");
    if (ret < 0) {
        QString msg = "udev_monitor_filter_add_match_subsystem_devtype";
        qCritical(device_category()) << msg;
        throw Device_exception(msg);
    }

    ret = udev_monitor_enable_receiving(udev_monitor);
    if (ret < 0) {
        QString msg = "udev_monitor_enable_receiving(udev_monitor) returned "
                + QString(ret);
        qCritical(device_category()) << msg;
        throw Device_exception(msg);
    }

    monitor_fd = udev_monitor_get_fd(udev_monitor);
    if (monitor_fd < 0) {
        QString msg = "mon_fd: " + QString(monitor_fd);
        qCritical(device_category()) << msg;
        throw Device_exception(msg);
    }
}

USB_device_scanner::~USB_device_scanner()
{
    udev_monitor_unref(udev_monitor);
    udev_unref(udev);
}

QString USB_device_scanner::scan()
{
    fd_set fdd_set;
    struct timeval timeout;

    timeout.tv_sec = 10;
    timeout.tv_usec = 0;

    FD_ZERO(&fdd_set);
    FD_SET(monitor_fd, &fdd_set);

    qDebug(device_category()) << ">>> SELECT <<<";

    int selectRetVal = select(monitor_fd + 1, &fdd_set, NULL, NULL, &timeout);

    if (selectRetVal == -1)
    {
        QString msg = "selectRetVal: -1";
        qCritical(device_category()) << msg;
        throw Device_exception(msg);

    }
    QString devpath = nullptr;
    if (FD_ISSET(monitor_fd, &fdd_set))
    {
        struct udev_device *dev = udev_monitor_receive_device(udev_monitor);

        if (! dev)
        {
            QString msg = "udev_monitor_receive_device() retruned NULL";
            qCritical(device_category()) << msg;
            throw Device_exception(msg);
        }

        if ((strncmp("add", udev_device_get_action(dev), 3) == 0) ||
                (strncmp("remove", udev_device_get_action(dev), 6) == 0)) {
            devpath = QString(udev_device_get_devpath(dev));
        }

        udev_device_unref(dev);
    }

    return devpath;
}
