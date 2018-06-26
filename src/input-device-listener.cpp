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
        usleep(10);
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
//static bool is_mouse_btn_pressed(struct input_event &e)
//{
//    cout << "e.type: " << e.type << "; e.code: " << e.code
//         << "; e.value: " << e.value << endl;
//    // XXX: The code should describe mouse button press, but it's always
//    //      equals to 4 for some reason.
//    return (e.type == EV_MSC) && (e.code == 4);
//}

static bool _loop_answer_mouse(string mouse)
{
    struct input_event ie;
    int bytes;
    int fd;

    mouse = FULLPATH + mouse;
    const char *pDevice = mouse.c_str();

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
        for (auto mouse : *devices)
        {
            if (_loop_answer_mouse(mouse))
            {
                return mouse;
            }
        }
    }
}
