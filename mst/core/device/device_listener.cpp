
#include "device_listener.h"
#include "device.h"

Q_LOGGING_CATEGORY(input_device_listener_category,
                   "mst.core.input_device_listener")

Device_listener::Device_listener(DEVICE_TYPE type)
{
    this->type = type;
    qRegisterMetaType<DEVICE_TYPE>();
}

// static methods ///////////////////////////////////////////////////////////////

static void _debug_print_devices(DEVICE_TYPE type, QVector<QString>* devices)
{
    switch (type) {
    case DEVICE_TYPE::KEYBOARD:
        qInfo(input_device_listener_category()) << "KEYBOARD: ";
        break;
    case DEVICE_TYPE::MOUSE:
        qInfo(input_device_listener_category()) << "MOUSE: ";
        break;
    case DEVICE_TYPE::USB:
        qInfo(input_device_listener_category()) << "USB: ";
        break;
    }

    for (auto d : *devices)
    {
        qInfo(input_device_listener_category()) << "     " << d;
    }
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
