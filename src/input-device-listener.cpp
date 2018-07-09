#include "input-device-listener.h"



Input_device_listener::Input_device_listener(vector<string> devices,
                                             DEVICE_TYPE type)
    : type(type)
{
    this->devices = new vector<string>(devices);
}

void Input_device_listener::run()
{
    string result;
    switch (type)
    {
    case DEVICE_TYPE::KEYBOARD:
        result = check_keybd();
        break;
    case DEVICE_TYPE::MOUSE:
        result = check_mice();
        break;
    }
    emit device_found(QString::fromStdString(result), type);
}

static bool is_btn_pressed(struct input_event &e)
{
    cout << "e.type: " << e.type << "; e.code: " << e.code
         << "; e.value: " << e.value << endl;
    return (e.type == EV_MSC) && (e.code == 4);
}

static bool _loop_answer_keybd(string keybd)
{
    struct input_event ie;
    int bytes;
    int fd;

    keybd = FULLPATH + keybd;
    const char *pDevice = keybd.c_str();

    fd = open(pDevice, O_RDWR);
    if (fd == -1)
    {
        throw (string)"ERROR Opening %s\n" + pDevice;
    }

    bytes = read(fd, (void *) &ie, sizeof(ie));
    if ((bytes > 0) && is_btn_pressed(ie))
    {
        char name[256] = "Unknown";
        ioctl (fd, EVIOCGNAME (sizeof (name)), name);
        cout << "Keybd: " << name << endl;
        return true;
    }

    close(fd);
    return false;
}

string Input_device_listener::check_keybd()
{
    while (1)
    {
        usleep(100);
        for (auto keybd : *devices)
        {
            if (_loop_answer_keybd(keybd))
            {
                return keybd;
            }
        }
    }
}

/**
 * @brief is_lmb_pressed -- Check if the left mouse button is pressed.
 * @param e -- An input event.
 * @return true or false.
 */
static bool is_mouse_btn_pressed(struct input_event &e)
{
    cout << "e.type: " << e.type << "; e.code: " << e.code
         << "; e.value: " << e.value << endl;
    // XXX: The code should describe mouse button press, but it's always
    //      equals to 4 for some reason.
    return (e.type == EV_MSC) && (e.code == 4);
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
    for (int count = 0; count < MAX_COUNT; ++count) {
        bytes = read(fd, (void *) ie, sizeof(struct input_event));
        if (bytes > 0)
            break;
        usleep(100);
    }
    return bytes;
}

static bool _loop_answer_mouse(string mouse)
{
    struct input_event ie;
    ssize_t bytes;
    int fd;
    cout << "_loop_answer_mouse: " + mouse << endl;
    mouse = FULLPATH + mouse;
    const char *pDevice = mouse.c_str();

    fd = open(pDevice, O_RDWR | O_NONBLOCK);
    if (fd == -1)
    {
        throw (string)"ERROR Opening " + pDevice;
    }

    bytes = _try_read(fd, &ie);

    if ((bytes > 0) && is_mouse_btn_pressed(ie))
    {
        char name[256] = "Unknown";
        ioctl (fd, EVIOCGNAME (sizeof (name)), name);
        cout << "Mouse: " << name << endl;
        return true;
    }

    close(fd);
    return false;
}

/**
 * @brief Input_device_listener::check_mice -- loop through the list of mice
 *    and get the one which buttons pressed.
 * @return An active mouse device name.
 */
string Input_device_listener::check_mice()
{
    while (1)
    {
        usleep(10);
        cout << "check_mice: Loop..." << endl;
        for (auto mouse : *devices)
        {
            if (_loop_answer_mouse(mouse))
            {
                return mouse;
            }
        }
    }
}
