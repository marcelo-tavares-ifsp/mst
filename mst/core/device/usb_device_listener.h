#ifndef USB_DEVICE_LISTENER_H
#define USB_DEVICE_LISTENER_H

#include <QObject>

#include "device_listener.h"

class USB_device_listener: public Device_listener
{
    Q_OBJECT

public:
    USB_device_listener(DEVICE_TYPE type);

private:
    device::USB_device_scanner scanner;
    QString poll();
};

#endif // USB_DEVICE_LISTENER_H
