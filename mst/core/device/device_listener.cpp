
#include "device_listener.h"
#include "device.h"

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
