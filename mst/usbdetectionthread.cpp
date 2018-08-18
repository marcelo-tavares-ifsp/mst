#include "usbdetectionthread.h"
#include <iostream>
#include <libudev.h>
#include "input-device-listener.h"

Q_LOGGING_CATEGORY(usb_detection_thread, "mst.usb_detection_thread")

using namespace std;

void UsbDetectionThread::run()
{
    qDebug(usb_detection_thread) << "running...";

    struct udev *udev = udev_new();

    if (!udev)
    {
        qCritical(usb_detection_thread) << "udev_new() returned NULL";
        return;
    }

    struct udev_monitor *udev_monitor = udev_monitor_new_from_netlink(udev, "udev");

    if (!udev_monitor)
    {
        qCritical(usb_detection_thread)
                << "udev_monitor_new_from_netlink() retruned NULL";
        return;
    }

    int retVal = udev_monitor_filter_add_match_subsystem_devtype(udev_monitor, "usb", "usb_device");

    if (retVal<0)
    {
        qCritical(usb_detection_thread)
                << retVal
                << " = udev_monitor_filter_add_match_subsystem_devtype"
                << "(udev_monitor, \"usb\", \"usb_device\");";
        return;
    }

    retVal = udev_monitor_enable_receiving(udev_monitor);

    if (retVal<0)
    {
        qCritical(usb_detection_thread)
                << retVal
                << " = udev_monitor_enable_receiving(udev_monitor);";
        return;
    }

    int monFd = udev_monitor_get_fd(udev_monitor);

    if (monFd < 0)
    {
        qCritical(usb_detection_thread) << "monFd: " << monFd;
        return;
    }

    while (true)
    {
        fd_set fdSet;
        struct timeval timeout;

        timeout.tv_sec = 10;
        timeout.tv_usec = 0;

        FD_ZERO(&fdSet);
        FD_SET(monFd, &fdSet);

        cout << ">>> SELECT <<<" << endl;

        int selectRetVal = select(monFd + 1, &fdSet, NULL, NULL, &timeout);

        if (selectRetVal == -1)
        {
            qCritical(usb_detection_thread) << "selectRetVal: -1";
            break;
        }
        else if (selectRetVal)
        {
            if (FD_ISSET(monFd, &fdSet))
            {
                struct udev_device *dev = udev_monitor_receive_device(udev_monitor);

                if (!dev)
                {
                    cout << "udev_monitor_receive_device() retruned NULL" << endl;
                    return;
                }

                if ((strncmp("add", udev_device_get_action(dev), 3) == 0) ||
                    (strncmp("remove", udev_device_get_action(dev), 6) == 0))
                {
                    emit usb_path_found(QString(udev_device_get_devpath(dev)), Input_device_listener::DEVICE_TYPE::USB);

                    udev_device_unref(dev);

                    break;
                }

                udev_device_unref(dev);
            }
        }
    }

    // ---

    udev_monitor_unref(udev_monitor);
    udev_unref(udev);
}
