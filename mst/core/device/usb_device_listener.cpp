#include "usb_device_listener.h"

USB_device_listener::USB_device_listener(DEVICE_TYPE type)
    : Device_listener(type)
{

}

QString USB_device_listener::poll()
{
    QString devpath = scanner.scan();
    if (devpath != nullptr) {
        return devpath;
    }
    return nullptr;
}


