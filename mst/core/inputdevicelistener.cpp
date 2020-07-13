
#include "inputdevicelistener.h"
#include "device.h"

Q_LOGGING_CATEGORY(input_device_listener_category, "mst.input_device_listener")

InputDeviceListener::InputDeviceListener(DEVICE_TYPE type, QVector<string> devices)
{
    this->type = type;
    this->devices = new QVector<string>(devices);
    qRegisterMetaType<DEVICE_TYPE>();
}

// static methods ///////////////////////////////////////////////////////////////

static void _debug_print_devices(DEVICE_TYPE type, QVector<string>* devices)
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
        qInfo(input_device_listener_category()) << "     " << d.c_str();
    }
}

bool InputDeviceListener::loop_answer_device(QString device)
{
    struct input_event ie;
    ssize_t bytes;
    int fd;

    device = QString::fromStdString(
                Path_manager::get_instance()->get_device_path())
            + device;
    const char *pDevice = device.toStdString().c_str();

    fd = open(pDevice, O_RDWR  | O_NONBLOCK);
    if (fd == -1)
    {
        QString message = "Could not open device: " + device;
        qCritical(input_device_listener_category()) << message;
        throw InputDeviceListener_exception(type, message);
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

// public methods ///////////////////////////////////////////////////////////////

void InputDeviceListener::run()
{
    string* result = NULL;
    is_running = true;
    _debug_print_devices(type, devices);
    sleep(5);
    switch (type)
    {
    case DEVICE_TYPE::KEYBOARD:
        result = check_device();
        break;
    case DEVICE_TYPE::MOUSE:
        result = check_device();
        break;
    case DEVICE_TYPE::USB:
        result = check_usb();
        break;
    }

    if (result)
    {
        emit device_found(to_qstring(*result), type);
        emit work_done();
    }
}

void InputDeviceListener::cancel()
{
    qInfo(input_device_listener_category()) << "Stopping the device thread...";
    is_running = false;
}

// private methods ///////////////////////////////////////////////////////////////

/**
 * @brief Input_device_listener::check_keybd -- loop through the list of
 *    keyboards and get the one which buttons pressed.
 * @return An active keyboard device name or NULL if the thread was stopped.
 */
string* InputDeviceListener::check_device()
{
    while (is_running)
    {
        usleep(100);
        for (auto device : *devices)
        {
            if (loop_answer_device(QString::fromStdString(device)))
            {
                return new string(device);
            }
        }
    }
    return NULL;
}

string* InputDeviceListener::check_usb()
{
    device::USB_device_scanner scanner;
    while (is_running)
    {
        QString devpath = scanner.scan();
        if (devpath != nullptr) {
            emit device_found(devpath, type);
            emit work_done();
            is_running = false;
        }
    }
    return NULL;
}
