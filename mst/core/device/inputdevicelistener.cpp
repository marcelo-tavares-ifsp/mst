
#include "inputdevicelistener.h"
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

Input_device_listener::Input_device_listener(DEVICE_TYPE type,
                                             QVector<QString> devices)
    : Device_listener(type)
{
    this->devices = new QVector<QString>(devices);
}

bool Input_device_listener::loop_answer_device(QString device)
{
    struct input_event ie;
    ssize_t bytes;
    int fd;

    string devpath = Path_manager::get_instance()->get_device_path()
                    + device.toStdString();

    const char *pDevice = devpath.c_str();

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
            is_pressed = device::is_btn_pressed(ie);
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

