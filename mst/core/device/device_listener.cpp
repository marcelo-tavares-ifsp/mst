
#include "device_listener.h"
#include "device.h"

#include <QApplication>

using namespace std;

Q_LOGGING_CATEGORY(input_device_listener_category,
                   "mst.core.input_device_listener")

Device_listener::Device_listener(DEVICE_TYPE type)
{
    this->type = type;
    qRegisterMetaType<DEVICE_TYPE>();
}

void Device_listener::run()
{
    QString device = nullptr;
    is_running = true;
    //_debug_print_devices(type, devices);
    sleep(5);
    while (is_running) {
        device = poll();
        if (device != nullptr) {
            emit device_found(device, type);
            emit work_done();
            is_running = false;
        }
    }
}

void Device_listener::cancel()
{
    qInfo(input_device_listener_category()) << "Stopping the device thread...";
    is_running = false;
}

const QString device::describe(DEVICE_TYPE type)
{
    switch (type) {
    case DEVICE_TYPE::KEYBOARD:
        return QApplication::translate("Device_listener", "Keyboard");
    case DEVICE_TYPE::MOUSE:
        return QApplication::translate("Device_listener", "Mouse");
    case DEVICE_TYPE::USB:
        return QApplication::translate("Device_listener", "USB");
    default:
        return nullptr;
    }
}
