#include "inputdevicelistener.h"

Q_LOGGING_CATEGORY(input_device_listener_category, "mst.input_device_listener")

InputDeviceListener::InputDeviceListener(DEVICE_TYPE type, vector<string> devices)
{
    this->type = type;
    this->devices = new vector<string>(devices);
    qRegisterMetaType<DEVICE_TYPE>();
}

// static methods ///////////////////////////////////////////////////////////////

static void _debug_print_devices(DEVICE_TYPE type, vector<string>* devices)
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

/**
 * @brief _try_read -- try to read data from a mouse in a loop.
 * @param fd -- mouse file descriptor.
 * @param ie -- input event.
 * @return -1 if there's no data to read, amount of data read otherwise.
 */
static ssize_t _try_read(int fd, struct input_event* ie) {
    static uint32_t MAX_COUNT = 100;
    ssize_t bytes = -1;
    for (uint32_t count = 0; count < MAX_COUNT; ++count) {
        bytes = read(fd, (void *) ie, sizeof(struct input_event));
        if (bytes > 0)
            break;
        usleep(100);
    }
    return bytes;
}

static bool is_btn_pressed(struct input_event &e)
{
    qInfo(input_device_listener_category()) << "e.type: "
        << e.type << "; e.code: " << e.code << "; e.value: " << e.value;
    return (e.type == EV_MSC) && (e.code == 4);
}

bool InputDeviceListener::loop_answer_device(string device)
{
    struct input_event ie;
    ssize_t bytes;
    int fd;

    device = PathManager::get_instance()->get_device_path() + device;
    const char *pDevice = device.c_str();

    fd = open(pDevice, O_RDWR  | O_NONBLOCK);
    if (fd == -1)
    {
        string message = "Could not open device: " + device;
        qCritical(input_device_listener_category()) << QString::fromStdString(message);
        throw InputDeviceListener_exception(type, message);
    }

    bytes = _try_read(fd, &ie);

    if (bytes > 0)
    {
        bool is_pressed;
        if (this->type == DEVICE_TYPE::MOUSE) {
            unsigned char* data = (unsigned char*) &ie;
            is_pressed = (data[0] & 0x1) || (data[0] & 0x2) || (data[0] & 0x4);
        } else {
            is_pressed = is_btn_pressed(ie);
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
            if (loop_answer_device(device))
            {
                return new string(device);
            }
        }
    }
    return NULL;
}

string* InputDeviceListener::check_usb()
{
    qDebug(input_device_listener_category()) << "running...";

    struct udev *udev = udev_new();

    if (!udev)
    {
        qCritical(input_device_listener_category()) << "udev_new() returned NULL";
        return NULL;
    }

    struct udev_monitor *udev_monitor = udev_monitor_new_from_netlink(udev, "udev");

    if (!udev_monitor)
    {
        qCritical(input_device_listener_category())
                << "udev_monitor_new_from_netlink() retruned NULL";
        return NULL;
    }

    int retVal = udev_monitor_filter_add_match_subsystem_devtype(udev_monitor, "usb", "usb_device");

    if (retVal<0)
    {
        qCritical(input_device_listener_category())
                << retVal
                << " = udev_monitor_filter_add_match_subsystem_devtype"
                << "(udev_monitor, \"usb\", \"usb_device\");";
        return NULL;
    }

    retVal = udev_monitor_enable_receiving(udev_monitor);

    if (retVal<0)
    {
        qCritical(input_device_listener_category())
                << retVal
                << " = udev_monitor_enable_receiving(udev_monitor);";
        return NULL;
    }

    int monFd = udev_monitor_get_fd(udev_monitor);

    if (monFd < 0)
    {
        qCritical(input_device_listener_category()) << "monFd: " << monFd;
        return NULL;
    }

    while (is_running)
    {
        fd_set fdSet;
        struct timeval timeout;

        timeout.tv_sec = 10;
        timeout.tv_usec = 0;

        FD_ZERO(&fdSet);
        FD_SET(monFd, &fdSet);

        qDebug(input_device_listener_category()) << ">>> SELECT <<<";

        int selectRetVal = select(monFd + 1, &fdSet, NULL, NULL, &timeout);

        if (selectRetVal == -1)
        {
            qCritical(input_device_listener_category()) << "selectRetVal: -1";
            break;
        }
        else if (selectRetVal)
        {
            if (FD_ISSET(monFd, &fdSet))
            {
                struct udev_device *dev = udev_monitor_receive_device(udev_monitor);

                if (!dev)
                {
                    qCritical(input_device_listener_category())
                            << "udev_monitor_receive_device() retruned NULL";
                    return NULL;
                }

                if ((strncmp("add", udev_device_get_action(dev), 3) == 0) ||
                    (strncmp("remove", udev_device_get_action(dev), 6) == 0))
                {
                    emit device_found(QString(udev_device_get_devpath(dev)), type);
                    emit work_done();

                    udev_device_unref(dev);

                    break;
                }

                udev_device_unref(dev);
            }
        }
    }

    udev_monitor_unref(udev_monitor);
    udev_unref(udev);
    return NULL;
}
