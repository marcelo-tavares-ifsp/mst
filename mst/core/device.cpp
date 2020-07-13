#include <unistd.h>
#include <linux/input.h>
#include <libudev.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include <QLoggingCategory>

#include "device.h"

using namespace std;
using namespace device;

Q_LOGGING_CATEGORY(device_category, "mst.core.device")

/**
 * @brief try_read -- try to read data from a mouse in a loop.
 * @param fd -- mouse file descriptor.
 * @param ie -- input event.
 * @return -1 if there's no data to read, amount of data read otherwise.
 */
ssize_t device::try_read(int fd, struct input_event* ie)
{
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

bool device::is_btn_pressed(struct input_event &e)
{
    qInfo(device_category()) << "e.type: "
        << e.type << "; e.code: " << e.code << "; e.value: " << e.value;
    return (e.type == EV_MSC) && (e.code == 4);
}

