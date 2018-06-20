#include "input-device-listener.h"

Input_device_listener::Input_device_listener(vector<string> devices,
                                             DEVICE_TYPE type)
    : devices(devices),
      type(type),
      QObject(parent)
{
    // https://forum.qt.io/topic/64760/gui-won-t-show-after-i-initialize-a-class-qobject/6
}

void Input_device_listener::run()
{
    string result;
    switch (type)
    {
    case DEVICE_TYPE::KEYBOARD:
        result = check_keyboards();
        break;
    case DEVICE_TYPE::MOUSE:
        result = check_mice();
        break;
    }
    emit device_found(result, type);
}

static bool _loop_answer_keybd(string keybd)
{
    int fd, bytes;
    string input = "";
    char *data;

    const char *pDevice = keybd.c_str();

    fd = open(pDevice, O_RDWR);
    if (fd == -1)
    {
        throw (string)"ERROR Opening %s\n" + pDevice;
    }

//    bytes = read(fd, data, sizeof(data));
//    if (data)
//    {
//        input = bytes;
//    }

//    std::transform(input.begin(), input.end(), input.begin(), ::tolower);
//    if (input == "this")
//    {
//        return true;
//    }

    return false;
}

string Input_device_listener::check_keyboards()
{
    while (1)
    {
        usleep(10);
        for (auto keybd : devices)
        {
            if (_loop_answer_keybd(keybd))
            {
                return keybd;
            }
        }
    }
}

static bool _loop_answer_mouse(string mouse)
{
//    const int BUF
//    _SZ = 10;
//    char buf[BUF_SZ];
//    FILE* file = open_input_dev(device);

//    while (fgets(buf, BUF_SZ, file) != NULL)
//    {
//        cout << buf << endl;    if (left > 0)
//    {
//        return true;
//    }
//    }



    int fd, bytes, left;
    unsigned char data[3];

    const char *pDevice = mouse.c_str();

    fd = open(pDevice, O_RDWR);
    if (fd == -1)
    {
        throw (string)"ERROR Opening %s\n" + pDevice;
    }

    bytes = read(fd, data, sizeof(data));
    if (bytes > 0)
    {
        left = data[0] & 0x1;
    }
    if (left > 0)
    {
        return true;
    }

//    pclose(file);

    return false;
}

string Input_device_listener::check_mice()
{
    while (1)
    {
        usleep(10);
        for (auto mouse : devices)
        {
            if (_loop_answer_mouse(mouse))
            {
                return mouse;
            }
        }
    }
}
